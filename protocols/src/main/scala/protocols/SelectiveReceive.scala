package protocols

import akka.actor.ActorLogging
import akka.actor.typed.{ActorContext, Behavior, ExtensibleBehavior, Signal}
import akka.actor.typed.scaladsl._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object SelectiveReceive {
    import akka.actor.typed.Behavior.{start, validateAsInitial, interpretMessage, canonicalize, isUnhandled}
  /**
    * @return A behavior that stashes incoming messages unless they are handled
    *         by the underlying `initialBehavior`
    * @param bufferSize Maximum number of messages to stash before throwing a `StashOverflowException`
    *                   Note that 0 is a valid size and means no buffering at all (ie all messages should
    *                   always be handled by the underlying behavior)
    * @param initialBehavior Behavior to decorate
    * @tparam T Type of messages
    *
    * Hint: Implement an [[ExtensibleBehavior]], use a [[StashBuffer]] and [[Behavior]] helpers such as `start`,
    * `validateAsInitial`, `interpretMessage`,`canonicalize` and `isUnhandled`.
    */
    def apply[T](bufferSize: Int, initialBehavior: Behavior[T]): Behavior[T] = {
      new Selective[T](initialBehavior, Queue[T](), bufferSize)
    }

  class Selective[T](behavior: Behavior[T], stash: Queue[T], bufferSize: Int) extends ExtensibleBehavior[T]{

    override def receive(ctx: ActorContext[T], msg: T): Behavior[T] = {
      println(s"new message arrives $msg")
      println(s"Current stash $stash")
      val (unhandled, newBeh, newCtx) = processMessage(msg, behavior, ctx)
      val unhdl = if (unhandled) 1 else 0
      if (stash.size + unhdl > bufferSize) throw new StashOverflowException("Overflow")
      println(s"processing the outstandings $stash")
      val newState = sendUntilNotChanged(stash, newBeh, newCtx)
      val newStash = if (unhandled) newState._1.enqueue(msg) else newState._1
      new Selective[T](newState._2, newStash, bufferSize)
    }

    override def receiveSignal(ctx: ActorContext[T], msg: Signal): Behavior[T] =
      Behavior.same

    def processMessage(msg: T, b: Behavior[T], ctx: ActorContext[T])
     :(Boolean, Behavior[T], ActorContext[T]) = {
      val started = validateAsInitial(start(b, ctx))
      val next = interpretMessage(started, ctx, msg)
      val unhandled = isUnhandled(next)
      println(s"processing message $msg witch is unhadleded $unhandled")
      (unhandled, canonicalize(next, b, ctx), ctx)
    }

    @tailrec
    private def sendUntilNotChanged(queue: Queue[T], behavior: Behavior[T], ctx: ActorContext[T]):
      (Queue[T], Behavior[T], ActorContext[T]) = {
      val newState = sendUntilReceived(queue, behavior, ctx)
      if (newState._1 == queue) newState
      else sendUntilNotChanged(newState._1, newState._2, newState._3)
    }

    @tailrec
    private def sendUntilReceived(toTry: Queue[T], behavior: Behavior[T], ctx: ActorContext[T], tried: Queue[T] = Queue[T]()):
    (Queue[T], Behavior[T], ActorContext[T]) = {
      toTry.dequeueOption match {
        case Some((msg, q)) =>
          val (unhandled, newBeh, newCtx) = processMessage(msg, behavior, ctx)
          if (unhandled) sendUntilReceived(q, newBeh, newCtx, tried.enqueue(msg))
          else (tried ++ q, newBeh, newCtx)
        case None => (tried, behavior, ctx)
      }
    }
  }
}
