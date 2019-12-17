package sandbox
import zio.{Task, ZIO}
import zio.interop.catz._
import cats._
import zio.blocking._
import cats.implicits._
import zio.DefaultRuntime
import zio.Exit
import zio.random.Random
import zio.duration._
import java.util.concurrent.TimeUnit
import java.time._
import zio.Fiber
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import zio.internal.Platform
import zio.internal.PlatformLive

object AsyncTesting //extends App 
                        {
    println("test")
    val t = Task(println("HI!"))
    val r = new DefaultRuntime{}
    
    r.unsafeRunAsync(t)(_ => ())

    val parallel = t.zipPar(t)
    
    r.unsafeRunAsync(parallel)(_ => ())

    def randomAsyncWorker(id: Int, time: Long) =
        for {
            _ <- ZIO{println(s"Start execution of task on async $id, with time $time on thread ${Thread.currentThread().getName()}")}
            _ <- ZIO.sleep(time.seconds)
            _ <- ZIO(println(s"Finish execution of task on $id, with time $time"))
        } yield id

    def randomBlockingWorker(id: Int, time: Long) =
        for {
            _ <- ZIO{println(s"Start execution of task on blocking $id, with time $time on thread ${Thread.currentThread().getName()}")}
            _ <- effectBlocking{Thread.sleep(time)}
            _ <- ZIO(println(s"Finish execution of task on $id, with time $time"))
        } yield id

    def sum(l: List[Int]) = ZIO{l.sum}
    val w1 = randomAsyncWorker(1, 1)
    val w2 = randomAsyncWorker(2, 3)

    val combined = for {
        f1 <- w1.fork
        f2 <- w2.fork
        r1 <- f1.await
        r2 <- f2.await
    } yield (r1, r2)

    // r.unsafeRunSync(combined)
    
    def combineAsync[A](l: List[ZIO[zio.clock.Clock, Throwable, A]]) = 
        l.parSequence.unit

    def generateAsyncTasks(size: Int) = 
        (0 until size).map{ i =>
            randomAsyncWorker(i, 1)
        }.toList

    // r.unsafeRunSync(combineAsync(generateAsyncTasks(10000)))

    def generateSyncTasks(size: Int) = 
        (0 until size).map{ i =>
            randomBlockingWorker(i, 1000)
        }.toList

    // r.unsafeRunSync(generateSyncTasks(100).parSequence.unit)
    val executor = Executors.newFixedThreadPool(1)
    val ec = ExecutionContext.fromExecutor(executor)
    val rt = new DefaultRuntime {
        override val platform: Platform = PlatformLive.fromExecutionContext(ec)
    }
    println("Single thread")
    // println("Non blocking")
    // rt.unsafeRunSync(generateAsyncTasks(3).parSequence.unit)
    val size = 10
    val asyncTasks = generateAsyncTasks(size)
    val syncTasks = generateSyncTasks(size)
    val blockingAndNonBlocking = generateAsyncTasks(size).parSequence <&> generateSyncTasks(size).parSequence
    // rt.unsafeRunSync(blockingAndNonBlocking.unit)

    val executeAllAsyncPar = ZIO.collectAllPar(asyncTasks)
    val executeAllSyncPar = ZIO.collectAllPar(syncTasks)
    val both = executeAllAsyncPar >>= (_ => executeAllSyncPar)

    rt.unsafeRun(both)

}