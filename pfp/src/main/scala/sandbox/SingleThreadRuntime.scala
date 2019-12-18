package sandbox
import zio.DefaultRuntime
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import zio.internal.{Platform, PlatformLive}

object SingleThreadRuntime {
    val executor = Executors.newFixedThreadPool(1)
    val ec = ExecutionContext.fromExecutor(executor)
    val r = new DefaultRuntime {
        override val platform: Platform = PlatformLive.fromExecutionContext(ec)
    }
}