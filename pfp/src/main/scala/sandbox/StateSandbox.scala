package sandbox
import cats.effect.concurrent.Ref
import cats.effect.Sync
import cats._
import cats.implicits._
import cats.effect.IO
import scala.concurrent.ExecutionContext

object StateSandbox //extends App 
{
    trait Counter[F[_]] {
        def incr: F[Unit]
        def get: F[Int]
    }

    class LiveCounter[F[_]] private (ref: Ref[F, Int]) 
        extends Counter[F]{
            def get: F[Int] = ref.get
            def incr: F[Unit] = ref.update(_ + 1)
    }
    object LiveCounter {
        def make[F[_]: Sync]: F[Counter[F]] =
            Ref.of[F, Int](0).map(new LiveCounter(_))
    }

    val counter = LiveCounter.make[IO]
    val action = IO{println("test")}
    val printCounter = IO{x: Int => println(s"counter is $x")}
    val program = 
    for {
        count <- counter
        _ <- count.incr
        _ <- count.incr
        _ <- action
        _ <- count.incr
        printer <- printCounter
        v <- count.get
    } yield printer(v)
    program.unsafeRunSync()

    implicit val ctx = IO.contextShift(ExecutionContext.global)
    
    def createWorker(id: Int, counter: Counter[IO])
        : IO[Unit] = for {
            v <- counter.get
            _ <- IO{println(s"worker $id got counter in state $v")}
            _ <- counter.incr
            v2 <- counter.get
            _ <- IO{println(s"worker $id incremented and got back $v2")}
        } yield ()

    val program2 = for {
        c <- counter
        w1 = createWorker(1, c)
        w2 = createWorker(2, c)
        w3 = createWorker(3, c)
        _ <- List(w1, w2, w3).parSequence.void
    } yield ()
    

    program2.unsafeRunSync()
    

}