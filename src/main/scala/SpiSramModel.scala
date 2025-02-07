
import scala.collection.mutable
import java.util.concurrent.atomic.AtomicBoolean

abstract class Model {

  val startPoint = new SynchronizationPoint()
  val endPoint = new SynchronizationPoint()

  var printDebug = false

  def debug(msg: String): Unit = {
    if (printDebug) println(msg)
  }

  

  var done = new AtomicBoolean(false)

  var cnt = 0

  val name = this.getClass().getSimpleName()

  def error(msg: String): Unit = throw new Exception(s"$name: $msg")

  def tick(): Unit = {
    debug(s"Controller lets ${name} tick")
    if (done.get()) return
    startPoint.reach()
    endPoint.waitUntilReached()
    debug(s"${name} is done ticking")
  }

  def isRunning: Boolean = !done.get()

  protected def step(n: Int = 1): Unit = {
    for (i <- 0 until n) {
      debug(s"${name} sleeping")
      endPoint.reach()
      startPoint.waitUntilReached()
      cnt += 1
      debug(s"${name} woke up for cycle $cnt")
    }
  }

  def body: Unit

  val thread = new Thread(new Runnable {
    override def run(): Unit = {
      debug(s"${name} waiting for start")
      startPoint.waitUntilReached()
      debug(s"${name} started")
      body
      done.set(true)
      endPoint.reach()
      debug(s"${name} exiting")
    }
  })

  thread.start()

}

class SpiSramModel(spi: SpiInterface) extends Model {
  import SpiSramModel._


  val txModel = new SpiSramTransactionModel()

  def body = {

    println("SRAM model started")

    waitForEnable()

    println("CE enabled")

    val command = getCommand().handleError {
      case EnableDeassertion => error("Chip Enable deasserted while receiving command")
      case UnknownCommand(id) => error(s"Received unknown command: $id")
    }
    println(s"Command: $command")

    val tx = if (command.hasAddress) {
      val address = getAddress().handleError {
        case EnableDeassertion => error("Chip Enable deasserted while receiving address")
      }
      println(s"Address: ${address.toHexString}")
      Request(command, Some(address))
    } else {
      Request(command, None)
    }

    println(s"Executing transaction: $tx")

    val response = txModel.execute(tx)

    println(s"Response: $response")

    response.data match {
      case Some((n, data)) => {
        println(s"Sending $n bytes of data: $data")
        sendBits(data, n * 8).handleError {
          case EnableDeassertion => error("Chip Enable deasserted while sending data")
        }
      }
      case None => ()
    }



  }

  def getCommand(): Result[Command, Error] = {
    sampleBits(8).andThen(Command.fromId)
  }

  def getAddress(): Result[Int, EnableDeassertion.type] = {
    sampleBits(24)
  }

  def waitForRisingEdge(): Result[Unit, EnableDeassertion.type] = {
    if (spi.getCe) return Err(EnableDeassertion)
    var i = 0
    while(spi.getSclk) {
      step()
      i += 1
      if (i > 10) error("SCLK stuck high")
      if (spi.getCe) return Err(EnableDeassertion)
    }
    i = 0
    while(!spi.getSclk) {
      step()
      if (spi.getCe) return Err(EnableDeassertion)
      i += 1
      if (i > 10) error("SCLK stuck low")
    }
    println("Rising edge")
    Ok(())
  }

  def waitForEnable(): Unit = {
    while (true) {
      if (!spi.getCe) return
      else step()
    }
  }

  def sampleBits(n: Int): Result[Int, EnableDeassertion.type] = {
    var value = 0
    for (i <- 0 until n) {
      if (waitForRisingEdge().isErr) return Err(EnableDeassertion)
      println(s"Slave reading bit ${i+1}/${n}")
      value = (value << 1) | spi.getSi
    }
    Ok(value)
  }

  def sendBits(value: BigInt, n: Int): Result[Unit, EnableDeassertion.type] = {
    for (i <- 0 until n) {
      spi.setSo(((value >> (n - 1 - i)) & 1).toInt)
      println(s"Slave waiting for rising edge for bit ${i+1}/${n}")
      if (waitForRisingEdge().isErr) return Err(EnableDeassertion)
      step(1)
    }
    Ok(())
  }
  
}


case class Request(command: SpiSramModel.Command, address: Option[Int])
case class Response(data: Option[(Int, BigInt)])

class SpiSramTransactionModel {

  val mem = mutable.Map[Int, Int]()

  val mfId = BigInt(0x0D)
  val kgd = BigInt(0x5D)
  val density = BigInt(0x2)
  val eid = BigInt(0x12345678)

  val id = mfId << 56 | kgd << 48 | density << 45 | eid


  def execute(request: Request): Response = {
    request.command match {
      case SpiSramModel.ReadId => Response(Some(2 + 9, id))
    }
  }



}


object SpiSramModel {

  object Command {
    def fromId(id: Int): Result[Command, UnknownCommand] = id match {
      case 0x9F => Ok(ReadId)
      case _ => Err(UnknownCommand(id))
    }
  }

  abstract class Command(id: Int) {

    def hasAddress: Boolean = this match {
      case ReadId => true
    }
  }
  case object ReadId extends Command(0x9F)

  sealed trait Error
  case object EnableDeassertion extends Error
  case class UnknownCommand(id: Int) extends Error
  
}


trait SpiInterface {
  def getSclk: Boolean
  def getCe: Boolean
  def getSi: Int
  def setSo(value: Int): Unit
}

class Assertion(condition: () => Boolean) {
  def check(): Unit = {
    if (!condition()) throw new AssertionError()
  }
}

object Assertion {
  def apply(condition: => Boolean): Assertion = new Assertion(() => condition)
}


class SynchronizationPoint {

  import java.util.concurrent.locks.{ReentrantLock, Condition}
  
  private val lock = new ReentrantLock()
  private val condition: Condition = lock.newCondition()
  private var reached = false

  def waitUntilReached(): Unit = {
    lock.lock()
    try {
      while (!reached) {
        condition.await()
      }
    } finally {
      reached = false
      lock.unlock()
    }
  }

  def reach(): Unit = {
    lock.lock()
    try {
      reached = true
      condition.signalAll()
    } finally {
      lock.unlock()
    }
  }
}