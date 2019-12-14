package sandbox

object FunctorApp {
    import cats.instances.function._
    import cats.syntax.functor._

    val func1: Int => Double = x => x.toDouble

    val func2: Double => Double = y => y * 2

    println((func1 map func2)(1))
    println((func1 andThen func2)(1))
    println(func2(func1(1)))

    import cats.Functor
    import cats.instances.list._
    import cats.instances.option._

    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)

    val option1 = Option(123)
    val option2 = Functor[Option].map(option1)(_.toString)

    println((list2, option2))

    val func3: Int => String = x => x.toString

    val func4 = Functor[List].lift(func3)

    def doMath[F[_]: Functor](start: F[Int]): F[Int] = 
        start.map(n => n + 1 * 2)

    import cats.instances.option._
    import cats.instances.list._

    println(doMath(Option(123)))
    println(doMath(Option.empty[Int]))
    println(doMath(List(3, 4, 5)))

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A])
        extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    implicit val TreeFunctor: Functor[Tree] = new Functor[Tree] {
        override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))
            case Leaf(a) => Leaf(f(a))
        }
    }

    val t: Tree[Int] = Branch(Branch(Leaf(8), Leaf(6)), Branch(Leaf(2), Branch(Leaf(3), Leaf(5))))
    println(t)
    println(t.map(_ * 2))
    
}