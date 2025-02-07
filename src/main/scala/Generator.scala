


class Generator[T] extends Iterable[T] {

  var stream = LazyList.empty[T]

  def emit(value: => T): Unit = {
    stream = stream #::: LazyList(value)
  }

  def emit(values: IterableOnce[T]): Unit = {
    stream = stream.appendedAll(values)
  }

  def iterator: Iterator[T] = stream.iterator

}



class MyGenerator extends Generator[Int] {

  emit(1)
  for (i <- 2 to 3) emit(i)
  emit(Seq(4, 5, 6))

}



object GeneratorTest extends App {
  val gen = new MyGenerator
  

  gen.foreach(println)

}