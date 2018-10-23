// Start writing your ScalaFiddle code here
object Option extends App {
import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this.flatMap(x => Some(f(x)))

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if (f(x)) Some[A](x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }

  def map3[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    x <- a
    y <- b
  } yield f(x, y)

  def map4[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(f(x, _)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
}

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(l:List[A], r: List[B]): Option[List[B]] = {
      l match {
        case Nil => Some(r)
        case h::t => f(h).flatMap(x => go(t, x::r))
      }
    }
    go(a.reverse,Nil)
    //a.foldRight[Option[List[B]]](Some(Nil))((h, end) => map2(f(h),end)(_::_))
  }
}

  val myOption1 = Some(2)
  val myOption2:Option[Int] = None
  println(myOption1.map(x => x * 2))
  println(myOption2.map(x => x * 2))

  println(myOption1.flatMap(x => Some(x * 3)))
  println(myOption2.flatMap(x => Some(x * 3)))

  println(myOption1.filter(_>1))
  println(myOption1.filter(_>3))
  println(myOption2.filter(_>1))

  println(myOption1.getOrElse(0))
  println(myOption2.getOrElse(0))

  println(myOption1.orElse(Some(0)))
  println(myOption2.orElse(Some(0)))

  println(Option.map2(myOption1, myOption1)(_+_))
  println(Option.map2(myOption1, myOption2)(_+_))

  println(Option.map3(myOption1, myOption1)(_+_))
  println(Option.map3(myOption1, myOption2)(_+_))

  println(Option.map4(myOption1, myOption1)(_+_))
  println(Option.map4(myOption1, myOption2)(_+_))

  println(Option.sequence(List(myOption1, myOption1, myOption1)))
  println(Option.sequence(List(myOption1, myOption2, myOption1)))

  println(Option.traverse(List(1,2,3,4))(x => if (x > 2) Some(x) else None))
  println(Option.traverse(List(1,2,3,4))(x => if (x > 0) Some(x) else None))

}
