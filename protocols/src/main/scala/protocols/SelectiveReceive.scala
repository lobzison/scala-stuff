package protocols

import akka.actor.typed.{ActorContext, ExtensibleBehavior, _}
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
    //TODO: Unstash
    def apply[T](bufferSize: Int, initialBehavior: Behavior[T]): Behavior[T] = {
        var stash = StashBuffer[T](30)
        new ExtensibleBehavior[T] {
            import akka.actor.typed.Behavior.{start, validateAsInitial, interpretMessage, canonicalize, isUnhandled}
            override def receive(ctx: ActorContext[T], msg: T): Behavior[T] = {
                val started = validateAsInitial(start(initialBehavior, ctx))
                val nextBehavior = interpretMessage(started, ctx, msg)
                if (isUnhandled(nextBehavior)) stash.stash(msg)
                canonicalize(nextBehavior, started, ctx)
            }

            override def receiveSignal(ctx: ActorContext[T], msg: Signal): Behavior[T] = initialBehavior
        }
    }
}
