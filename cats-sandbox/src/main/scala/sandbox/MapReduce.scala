package sandbox
import cats._
import cats.implicits._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MadReduce extends App {
    println("test")

    def foldMap[A, B: Monoid](s: Vector[A])(f: A => B): B = 
        s.foldLeft(Monoid[B].empty)(_ |+| f(_))

    println(foldMap(Vector(1, 2, 3))(identity))

    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))

    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

    println(Runtime.getRuntime.availableProcessors)

    val a = Vector(12,2,3)


    def parallelFoldMap[A, B: Monoid]
        (values: Vector[A])
        (f: A => B): Future[B] = {
            val cpuCount = Runtime.getRuntime.availableProcessors
            val batchSize = (values.length / cpuCount) + 1
            val batches = values.grouped(batchSize).toVector
            batches
                .traverse(x => Future(foldMap(x)(f)))
                .map(_.combineAll)
        }

    val result: Future[Int] = 
        parallelFoldMap((1 to 10000000).toVector)(identity)

    Await.result(result, 1.second)
    println(result)
}