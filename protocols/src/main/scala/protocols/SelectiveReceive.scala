package protocols

import akka.actor.typed.{Behavior, ExtensibleBehavior, Signal, ActorContext}
import akka.actor.typed.scaladsl._

object SelectiveReceive {
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
    //TODO: Fuck the StashBuffer. It's not immutable, it don't have a way to remove head, it don't have fold.
    //Screw this, I'll fix it with the Queue.
    import akka.actor.typed.Behavior.{start, validateAsInitial, interpretMessage, canonicalize, isUnhandled}
    def apply[T](bufferSize: Int, initialBehavior: Behavior[T]): Behavior[T] = {
      new Selective[T](initialBehavior, StashBuffer[T](bufferSize))
    }

  class Selective[T](behavior: Behavior[T], stash: StashBuffer[T])
    extends ExtensibleBehavior[T] {
    override def receive(ctx: ActorContext[T], msg: T): Behavior[T] = {
      val started = validateAsInitial(start(behavior, ctx))
      val next = interpretMessage(started, ctx, msg)
      if (isUnhandled(next)) {
        new Selective[T](canonicalize(next, behavior, ctx), stash.stash(msg))
      } else {
        new Selective[T](canonicalize(next, behavior, ctx), stash)
      }
    }

    override def receiveSignal(ctx: ActorContext[T], msg: Signal): Behavior[T] =
      Behavior.same

  }
}
