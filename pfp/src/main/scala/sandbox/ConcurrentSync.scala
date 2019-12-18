package sandbox
import cats._
import cats.implicits._
import zio.{Task, ZIO, UIO}
import zio.duration._
import zio.DefaultRuntime
import zio.Queue
import zio.IO
import zio.Schedule
import java.util.Calendar
import zio.clock._
import sandbox.SingleThreadRuntime
object ConcurrentSync extends App {
    println("test")

    def delay(duration: Long) = ZIO.sleep(duration.seconds)
    
    def log(s: String) = for {
        time <- currentDateTime
        res <- ZIO(println(s"$time | $s"))
    } yield res
    
    
    def producer(name: String, delayDur: Long, succeedVal: Int = 42) = 
        log(s"$name started work on ${Thread.currentThread().getName()}") *>
        delay(delayDur) *>
        log(s"$name ended work and returns value") *>
        ZIO.succeed(succeedVal)
    
    def consumer(name: String, delayDur: Long) = {
        log(s"$name started consuming on ${Thread.currentThread().getName()}") *>
        delay(delayDur) *> 
        ZIO.succeed((x: Int) => log(s"$name consumed value $x on ${Thread.currentThread().getName()}"))
    }
        

    val slowProducerWorker = producer("first", 1L)
    val fastProducerWorker = producer("first", 0L)
    val fastConsumerWorker = consumer("first", 0L)
    val slowConsumerWorker = consumer("first", 1L)

    val r = new DefaultRuntime{}
    val pipe = for {
        p <- slowProducerWorker
        c <- fastConsumerWorker
    } yield c(p)

    val pipePrinter = pipe.map(println(_))
    // r.unsafeRun(pipePrinter)

    val q = Queue.bounded[Int](1)
    def queueProducer(name: String, delayDur: Long, queue: Queue[Int]) =
        for {
            v <- producer(name, delayDur)
            res <- queue.offer(v)
        } yield res

    def queueConsumer(name: String, delayDur: Long, queue: Queue[Int]) = 
        for {
            item <- queue.take
            cons <- consumer(name, delayDur)
            _ <- cons(item)
        } yield ()
    
    def schedule(interval: Int) = Schedule.spaced(interval.second).jittered()
    val scheduledProducer = 
        (q:Queue[Int]) => queueProducer("producer 1", 2, q).repeat(schedule(3))

    val scheduledConsumer = 
        (q:Queue[Int]) => queueConsumer("consumer 1", 0, q).repeat(schedule(1))

    val queuePipe = for {
        queue <- q
        res <- scheduledProducer(queue) <&> scheduledConsumer(queue)
    } yield res

    val a = ZIO(println("qwe")).repeat(Schedule.forever)

    // r.unsafeRun(queuePipe)

    // val b = currentDateTime
    // r.unsafeRun(b.map(println(_)))

    SingleThreadRuntime.r.unsafeRun(queuePipe)

}