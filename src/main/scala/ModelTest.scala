
class SpiTestInterface extends SpiInterface {
  
  var sclk = false
  var ce = true
  var si = 0
  var so = 0

  def getSclk = sclk
  def getCe = ce
  def getSi = si
  def setSo(value: Int): Unit = so = value

  def setSclk(value: Boolean) = sclk = value
  def setCe(value: Boolean) = ce = value
  def setSi(value: Int) = si = value


  override def toString(): String = {
    s"SCLK: $sclk CE: $ce SI: $si SO: $so"
  }
}


class SpiMaster(spi: SpiTestInterface) extends Model {

  def body = {

    println("Master started")

    step(3)

    spi.setCe(false)

    step(2)

    println("Write command")
    sendBits(0x9F, 8)

    println("write address")
    sendBits(0, 24)

    println("Read response")
    val rx = receiveBits(11 * 8)

    println(s"Response: ${rx.toString(16)}")

    val mfid = rx >> 56
    val kgd = (rx >> 48) & 0xFF
    val density = (rx >> (48 - 3)) & 0x7
    val eid = rx & 0x1FFFFFFFFL

    println(s"MFID: ${mfid.toString(16)}")
    println(s"KGD: ${kgd.toString(16)}")
    println(s"Density: ${density.toString(16)}")
    println(s"EID: ${eid.toString(16)}")

  }

  def sendBits(value: Int, n: Int): Unit = {
    for (i <- 0 until n) {
      println(s"Master sending bit ${i+1}/${n}")
      spi.setSi((value >> (n - 1 - i)) & 1)
      step(1)
      spi.setSclk(false)
      step(2)
      spi.setSclk(true)
      step(1)
    }
    step(1)
  }

  def receiveBits(n: Int): BigInt = {
    var rx = BigInt(0)
    for (i <- 0 until n) {
      spi.setSclk(false)
      step(2)
      spi.setSclk(true)
      println(s"Master receiving bit ${i+1}/${n}")
      rx = (rx << 1) | spi.so
      step(2)
    }
    rx
  }

}

object ModelTest extends App {
  val spi = new SpiTestInterface
  val model = new SpiSramModel(spi)
  val master = new SpiMaster(spi)

  val vcd = new VCDWriter("spi.vcd", "top", Seq(
    VcdVariable("sclk", "wire", 1, "b0"),
    VcdVariable("ce", "wire", 1, "b1"),
    VcdVariable("si", "wire", 1, "b0"),
    VcdVariable("so", "wire", 1, "b0"),
  )
  )
  
  println("Start")

  var i = 0
  while (model.isRunning || master.isRunning) {
    println(s"===========@${i}ns=============")
    

    master.tick()
    model.tick()
    
    

    //println(spi)
    vcd.valueChange("sclk", if(spi.getSclk) 1 else 0)
    vcd.valueChange("ce", if(spi.getCe) 1 else 0)
    vcd.valueChange("si", spi.getSi)
    vcd.valueChange("so", spi.so)
    vcd.tick()
    i += 1
  }

  vcd.close()
  println("End")
}
