// 3
//package fpinscala.datastructures
object fp3 extends App {
sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_+_)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_*_)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((h, end) => Cons(h, end))

  def appendOld[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, appendOld(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(head, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go (n:Int, ls: List[A]): List[A] = {
      ls match {
          case Cons(h, t) if n > 0 => go(n-1, t)
          case _ => ls
      }
    }
    go(n, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    def go(l: List[A], res: List[A]): List[A] = {
      l match {
        case Cons(h, t@Cons(_, _)) => go(t, Cons(h, res))
        case _ => res
      }
    }
    go(l, Nil)
  }

  def length[A](l: List[A]): Int = {
    List.foldLeft(l, 0)((end, _) => end + 1)
  }

  def range(low: Int, hi: Int): List[Int] = {
    @annotation.tailrec
    def go(l:List[Int], n:Int):List[Int] = {
      if (n < hi) go(Cons(n, l), n+1)
      else l
    }
    go(Nil, low)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(end:B, l:List[A]): B = {
      l match {
        case Nil => end
        case Cons(h, t) => go(f(end, h), t)
      }
    }
    go(z, l)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((h, end) => f(end, h))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((end, h) => Cons(h, end))

  def flatten[A](l:List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, end) => Cons(f(h), end))

  def add1(l: List[Int]): List[Int] = map(l)(_+1)

  def listToString(l: List[Double]): List[String] = map(l)(_.toString())

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((h, end) => if (f(h)) Cons(h, end) else end)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((h, end) => append(f(h), end))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(h1+h2, addLists(t1, t2))
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
  }

}

  val myList = List(1,2,3)
  val myList2 = List(1,2,3,4,5,6,7,8)
  val myTail = List.tail(myList)
  println(myList)
  println(myTail)
  val mySet = List.setHead(myTail, 3)
  println(mySet)
  println(List.drop(myList, 4))
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(List.foldLeft(List(1,2,3), Nil:List[Int])((end, h) => Cons(h,end)))
  //println(List.length(myList))
  val myLongList = List.range(1, 50000)
  //blows the stack
  //not anymore
  println(List.foldRight(myLongList, 0)(_+_))
  //works just fine
  println(List.foldLeft(myLongList, 0)(_+_))
  println(List.reverse(myList))
  println(List.append(List(1,2,3), List(4, 5, 6)))
  println(List.appendOld(List(1,2,3), List(4, 5, 6)))
  println(List.flatten(List(List(1,2), List(3,4), List(5,6))))
  println(List.map(myList)(_*2))
  println(List.filter(myList2)(_ % 2 == 0))
  println(List.flatMap(myList2)(List(_)))
  println(List.filter2(myList2)(_ % 2 == 0))
  println(List.addLists(List(1,2,3), List(4,5,6)))
  println(List.zipWith(List(1,2,3), List(4,5,6))((_,_)))


}