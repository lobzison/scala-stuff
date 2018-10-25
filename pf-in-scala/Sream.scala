object Stream extends App {
import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n == 0 => empty
    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n==0 => this
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this.foldRight[Stream[A]](empty)((h, end) => if (p(h)) cons(h, end) else end)
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] = this.foldRight(None:Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(Empty:Stream[B])((h, end) => cons(f(h), end))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(Empty:Stream[A])((h, end) => if (f(h)) cons(h, end) else end)

  def flatMap[B](f: A => Stream[B]):Stream[B] = this.foldRight(Empty:Stream[B])((h, end) => f(h).foldRight(end)((h, end) => cons(h, end)))

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList:List[A] = this.foldRight(Nil:List[A])((h:A, t) => h::t)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

  val myStream = Stream(34+22, 18*9, 11/3)
  println(myStream)
  println(myStream.toList)
  println(myStream.take(2).toList)
  println(myStream.drop(2).toList)
  println(myStream.take(5).toList)
  println("takeWhile")
  println(myStream.takeWhile(_ > 10).toList)
  val myStream2 = Stream(1,10,10,{println("This shold not happen"); 1}, 2)
  println(myStream2.forAll(_<100))
  val myStream3 = Stream()
  println(myStream2.headOption)
  println(myStream3.headOption)

  println(myStream.map(_*2).toList)

  println(myStream2.map(_*2).toList)

  println(myStream.filter(_%2==0).toList)

  val f: Int => Stream[Int] = x => Stream(x, x)

  println(myStream2.flatMap(f).toList)
}
