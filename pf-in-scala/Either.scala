object Eiher extends App {
  import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this.flatMap(x => Right(f(x)))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case x:Left[EE] => x
      case Right(x) => f(x)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case x:Left[EE] => b
      case _ => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      a <- this
      b <- b
    } yield f(a, b)

  }

  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {

    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = Either.traverse(es)(x => x)

    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

    def traverse[E,A,B](as:List[A])(f: A => Either[E,B]):Either[E, List[B]] = {
      as.foldRight[Either[E,List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_::_))
    }

  }

  val myEitherL = Left("Error in processing")
  val myEitherR = Right(42)

  println(myEitherR.flatMap((x:Int) => Right(x + 42)))
  println(myEitherL.flatMap((x:Int) => Right(x + 42)))

  println(myEitherR.map((x:Int) => x + 42))
  println(myEitherL.map((x:Int) => x + 42))

  println(myEitherR.map2(myEitherR)((x, y) => x - y))
  println(myEitherR.map2(myEitherL)((x:Int, y: Int) => x - y))

  println(Either.traverse(List(42,13,22))(x => Either.safeDiv(32, x)))
  println(Either.traverse(List(42,13,22,0))(x => Either.safeDiv(32, x)))

  println(Either.sequence(List(myEitherR,myEitherR,myEitherR)))
  println(Either.sequence(List(myEitherR,myEitherL,myEitherR)))

}
