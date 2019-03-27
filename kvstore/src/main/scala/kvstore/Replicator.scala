package kvstore

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
//import akka.event.LoggingReceive
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging{
  import Replicator._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq(): Long = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def receive: Receive = /*LoggingReceive*/{
    case x@Replicate(k, v, _) =>
      val sq = nextSeq()
      acks += sq -> ((sender, x))
      replica ! Snapshot(k, v, sq)
    case SnapshotAck(_, sq) =>
      acks.get(sq) foreach {case (s, r) =>
          acks -= sq
          s ! Replicated(r.key, r.id)
      }
  }

  context.system.scheduler.schedule(100 millis, 100 millis) {
    acks.foreach{
      case (s, (_, r)) => replica ! Snapshot(r.key, r.valueOption, s)
    }
  }

}
