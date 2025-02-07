


sealed trait Result[+T, +E] {
  def isOk: Boolean = {
    this match {
      case Ok(_) => true
      case Err(_) => false
    }
  }
  def isErr: Boolean = {
    this match {
      case Ok(_) => false
      case Err(_) => true
    }
  }
  def andThen[B,F >: E](f: T => Result[B,F]): Result[B, F] = {
    this match {
      case Ok(value) => f(value)
      case Err(error) => Err(error)
    }
  }
  def map[B](f: T => B): Result[B, E] = {
    this match {
      case Ok(value) => Ok(f(value))
      case Err(error) => Err(error)
    }
  }

  def handleError(f: E => Unit): T = {
    this match {
      case Ok(value) => value
      case Err(error) => f(error); throw new RuntimeException("Error")
    }
  }
}

case class Ok[T](value: T) extends Result[T, Nothing]
case class Err[E](error: E) extends Result[Nothing, E]

object Result {
  def ok[T](value: T): Result[T, Nothing] = Ok(value)
  def err[E](error: E): Result[Nothing, E] = Err(error)
}


