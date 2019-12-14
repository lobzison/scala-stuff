package sandbox

object MonadApp {
    import scala.language.higherKinds 
    trait Monad[F[_]] {
        def pure[A](a: A): F[A]
        def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
        def map[A, B](value: F[A])(func: A => B): F[B] =
            flatMap(value)(x => pure(func(x)))
    }
    // import cats.Monad
    // import cats.instances.int._
    // import cats.syntax.applicative._
    // import cats.syntax.functor._
    // import cats.syntax.flatMap._
    // import cats.instances.option._
    // import cats.instances.list._
    // 1.pure[Option]
    // // res4: Option[Int] = Some(1)
    // 1.pure[List]
    // // res5: List[Int] = List(1)
    // def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    //     a.flatMap(x => b.map(y => x*x + y*y))
    
    // println(sumSquare(Option(2), Option(3)))
    // println(sumSquare(List(1,2,3), List(4, 5, 6)))
    import cats.Id
    val test = 3:Id[Int]
    // println(sumSquare(3: Id[Int], 4: Id[Int]))

    implicit val idMonad = new Monad[Id] {
        def flatMap[A, B](value: cats.Id[A])(func: A => cats.Id[B]): cats.Id[B] = func(value)
        def pure[A](a: A): cats.Id[A] = a
        override def map[A, B](value: cats.Id[A])(func: A => B): cats.Id[B] = flatMap(value)(func)
    }

    import cats.Eval

    def factorial(n: BigInt): Eval[BigInt] = {
        if (n==1){
            Eval.now(n)
        } else {
            Eval.defer(factorial(n - 1)).map(_ * n)
        }
    }

    // println(factorial(50000).value)
    def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail =>
        Eval.defer(fn(head, foldRight(tail, acc)(fn)))
    case Nil =>
      acc
    }

    val test2 = (1 to 50000).toList
    println(foldRight(test2, Eval.now(0)){case (v, acc) => acc.map(_ + v)}.value)

    import cats.data.Writer



    val writer = Writer(Vector("It was the best of times", "It was the worst of times")
            , 1859)
    

}