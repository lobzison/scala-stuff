package kvstore

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import kvstore.Arbiter._
import scala.concurrent.duration._
import scala.language.postfixOps
//import akka.event.LoggingReceive


object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with ActorLogging{
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  val persister: ActorRef = context.system.actorOf(persistenceProps)
  // Map from request id to pair of requester and snapshot to persist
  // Persistance of operation is done when map don't contain the id anymore
  var toPersist: Map[Long, (ActorRef, Snapshot)] = Map[Long, (ActorRef, Snapshot)]()
  // Map from request id tp triplet of requester, replicate message to be processed, and replicators who did not ack'ed replication
  // Persistance of operation is done when set of replicators is empty
  var toReplicate: Map[Long, (ActorRef, Replicate, Set[ActorRef])] = Map[Long, (ActorRef, Replicate, Set[ActorRef])]()

  arbiter ! Join


  def receive: PartialFunction[Any, Unit] = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = /*LoggingReceive*/{
    case Insert(k, v, id) =>
      replicatePersistPrimary(id, k, Some(v))
    case Remove(k, id) =>
      replicatePersistPrimary(id, k, None)
    case Get(k, id) => sender ! GetResult(k, kv.get(k), id)
    case Persisted(_, id) =>
      toPersist.get(id) foreach {case (to, snapshot) =>
          toPersist -= id
          if (isAck(id)) {
            updateKV(snapshot.key, snapshot.valueOption)
            to ! OperationAck(id)
          }
      }
    case Replicated(_, id) =>
      toReplicate.get(id) foreach {case (to, msg, set) =>
          val newSet = set - sender
          if (newSet.isEmpty) {
            toReplicate -= id
            if (isAck(id)) {
              updateKV(msg.key, msg.valueOption)
              to ! OperationAck(id)
            }
          } else {
            toReplicate += id -> ((to, msg, newSet))
          }
      }
    case Replicas(replicas) =>
      val replicasNew = replicas - self
      val leavers = secondaries.keySet -- replicasNew
      val newJoiners = replicasNew -- secondaries.keySet
      //kill leavers
      leavers foreach {x =>
          secondaries(x) ! PoisonPill
          x ! PoisonPill
      }
      //clean awaiting replications
      val replicatorLeavers = for {
        leave <- leavers
      } yield secondaries(leave)
      toReplicate =
      toReplicate.map{
        case (id, (to, rep, set)) => (id, (to, rep, set -- replicatorLeavers))
      }
      //send acks for deleted ones
      toReplicate foreach{
        case (id, (to, _, _)) if isAck(id) => to ! OperationAck(id)
        case _ => ()
      }
      //update secondaries and replicators
      secondaries = secondaries ++
        newJoiners
        .map(x => (x,context.system.actorOf(Props(classOf[Replicator], x))))
        .toMap -- leavers
      replicators = secondaries.values.toSet
      //send current state to new joiners
      val replicatorJoiners = for {
        join <- newJoiners
      } yield secondaries(join)
      kv foreach{
        case (k, v) =>
          val id = replSequence()
          val msg = Replicate(k, Some(v), id)
          toReplicate += id -> ((self, msg, replicatorJoiners))
          sendReplicateToAll(id)
      }
  }

  var expected = 0L

  val replica: Receive = /*LoggingReceive*/{
    case Get(k, id) => sender ! GetResult(k, kv.get(k), id)
    case Snapshot(k, _, seq) if seq < expected => sender ! SnapshotAck(k, seq)
    case s@Snapshot(k, v, seq) if seq == expected =>
      toPersist += seq -> ((sender, s))
      updateKV(k, v)
      persister ! Persist(k, v, seq)
    case Persisted(k, seq) =>
      toPersist.get(seq) foreach {case (to, _) =>
          toPersist -= seq
          expected = expected max (seq + 1)
          to ! SnapshotAck(k, seq)
      }
  }

  //repeat persistance
  context.system.scheduler.schedule(100 millis, 50 millis) {
    toPersist.foreach {
      case (_, (_, s)) => persister ! Persist(s.key, s.valueOption, s.seq)
    }
  }

  def scheduleFailPersist(id: Long): Unit = {
    context.system.scheduler.scheduleOnce(1 second){
      toPersist.get(id) foreach {case (to, _) =>
        toPersist -= id
        toReplicate -= id
        to ! OperationFailed(id)
      }
    }
  }

  def scheduleFailReplicate(id: Long): Unit = {
    context.system.scheduler.scheduleOnce(1 second){
      toReplicate.get(id) match {
        case Some((to, _, set)) if set.nonEmpty =>
          toReplicate = toReplicate - id
          toPersist = toPersist - id
          to ! OperationFailed(id)
        case _ => Unit
      }
    }
  }

  def sendReplicateToAll(id: Long): Unit = {
    toReplicate.get(id) foreach { case (_, msg, set) =>
        set foreach(_ ! msg)
    }
  }

  def persistPrimary(id: Long, k: String, v: Option[String]): Unit = {
    toPersist += id -> ((sender, Snapshot(k, v, id)))
    persister ! Persist(k, v, id)
    scheduleFailPersist(id)
  }

  def replicatePrimary(id: Long, k: String, v: Option[String]): Unit = {
    if (replicators.nonEmpty) toReplicate += id -> ((sender, Replicate(k, v, id), replicators))
    replicators foreach (_ ! Replicate(k, v, id))
    scheduleFailReplicate(id)
  }

  def replicatePersistPrimary(id: Long, k: String, v: Option[String]): Unit = {
    persistPrimary(id, k, v)
    replicatePrimary(id, k, v)
  }



  def isPersisted(id: Long): Boolean = {
    !toPersist.contains(id)
  }

  def isReplicated(id: Long): Boolean = {
    toReplicate.get(id) match {
      case Some((_, _, set)) => set.isEmpty
      case None => true
    }
  }

  def isAck(id: Long): Boolean = isPersisted(id) && isReplicated(id)
  def scheduleFail(id: Long): Unit = {
    scheduleFailPersist(id)
    scheduleFailReplicate(id)
  }

  def updateKV(k: String, valueOption: Option[String]): Unit = {
    valueOption match {
      case Some(v) => kv += k -> v
      case None => kv -= k
    }
  }

  private var replCounter = -1L
  def replSequence(): Long = {
    val ret = replCounter
    replCounter -= 1
    ret
  }
}

