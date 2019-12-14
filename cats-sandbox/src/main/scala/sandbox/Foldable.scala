package sandbox
import cats._
import cats.implicits._

object FoldableApp {
    println("test")
    val test1 = List(1,2,3,4)
    println(test1.foldLeft[List[Int]](Nil)((acc, i) => i :: acc))
    println(test1.foldRight[List[Int]](Nil)((i, acc) => i :: acc))

    def sum[A](l: List[A])(implicit m: Monoid[A]) = 
        l.foldRight(m.empty)(m.combine)
    
    println(sum(List(1,2,3,4,5)))
    
    def filter[A](l: List[A], f: A => Boolean) =
        l.foldRight(List.empty[A]){
            (i, acc) => if (f(i)) i :: acc else acc
        }
    println(filter[Int](List(3,-1,0,-13,13), (_ > 0)))
    println(filter[Int](List(-1,0,-13), (_ > 0)))
    
    def flatMap[A, B](l: List[A], f: A => List[B]): List[B] = 
        l.foldRight(List.empty[B]){(i, acc) => 
            f(i) ::: acc
        }

    println(flatMap[Int, Int](List(2,4,6,8), x => List(x / 2, x)))

    def map[A, B](l: List[A], f: A => B): List[B] =
        l.foldRight(List.empty[B]){(i, acc) =>
            f(i) :: acc
        }

    println(map[Int, String](test1, x => s"this is $x"))
    
    println(List(1,2,3).combineAll)

    def listTraverse[F[_]: Applicative, A, B]
    (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _)
    }
    def listSequence[F[_]: Applicative, B] (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

    println(listSequence(List(Vector(1, 2), Vector(3, 4))))

}