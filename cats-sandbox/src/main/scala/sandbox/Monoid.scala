package sandbox

object MonoidApp {
    // trait Monoid[A]{
    //     def combine(one: A, other: A): A
    //     def empty: A
    // }
    // object Monoid {
    //     def apply[A](implicit m: Monoid[A]) = m
    // }
    // object MonoidOps{
    //     implicit class MonoidSyntax[A](a: A)(implicit m: Monoid[A]) {
    //         def combine(other: A) = m.combine(a, other)
    //         def empty = m.empty
    //     }
    // }
    // object MonoidInstances {
    //     implicit val intMonoidInstance = new Monoid[Int] {
    //         def combine(one: Int, other: Int): Int = one + other
    //         def empty: Int = 0
    //     }
    // }

    // import MonoidInstances._
    // import MonoidOps._

    // println(1 combine 2)
    // println(1 combine 1.empty)
    import cats.Monoid
    import cats.instances.string._
    import cats.instances.option._
    import cats.instances.int._
    import cats.syntax.semigroup._
    // import cats._
    // import cats.implicits._
    
    println(Monoid[String].combine("asd","dfg"))
    println("asd" |+| "ewq")
    println(Option(12) |+| Option(22))

    def add[A: Monoid](items: List[A]): A = 
        items.foldLeft(Monoid[A].empty)(_ |+| _)

        
    println(add[Int](List(4, 5, 2, 1, 0, -1)))
    import cats.instances.int._
    import cats.instances.option._
    println(add[Option[Int]](List(Option(5), Option(2), Option(1), Option.empty)))
    case class Order(totalCost: Double, quantity: Double)
    implicit val orderMonoid = new Monoid[Order] {
        def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
        def empty: Order = Order(0 ,0)
    }
    println(add(List(Order(12, 2), Order(13, 3))))
}