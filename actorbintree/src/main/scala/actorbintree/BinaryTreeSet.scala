/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.event.LoggingReceive

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor{
  import BinaryTreeSet._
  import BinaryTreeNode._


  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = LoggingReceive {
    case x: Operation => root ! x
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }}

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case x: Operation => pendingQueue = pendingQueue.enqueue(x)
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot
      pendingQueue foreach(root ! _)
      pendingQueue = Queue.empty[Operation]
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with akka.actor.ActorLogging{
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = LoggingReceive{
    case x:Insert => handleInsert(x)
    case x: Contains => handleContain(x)
    case x: Remove => handleRemove(x)
    case c@CopyTo(a) => handleCopy(a, c)}

  def handleContain(c: Contains): Unit = {
    c match {
      case Contains(requester, id, elemReq) =>
        if (elemReq == elem)
          if (!removed) requester ! ContainsResult(id, true)
          else requester ! ContainsResult(id, false)
        else if (elemReq < elem)
          if (subtrees.contains(Left)) subtrees(Left) ! c
          else requester ! ContainsResult(id, false)
        else
          if (subtrees.contains(Right)) subtrees(Right) ! c
        else requester ! ContainsResult(id, false)
    }
  }

  def handleInsert(i: Insert): Unit = {
    i match {
      case Insert(requester, id, elemReq) => if (elemReq > elem) {
        if (subtrees.contains(Right)) subtrees(Right) ! i
        else {
          subtrees = subtrees + (Right -> context.actorOf(BinaryTreeNode.props(elemReq, false)))
          requester ! OperationFinished(id)
        }
      } else if (elemReq < elem) {
        if (subtrees.contains(Left)) subtrees(Left) ! i
        else {
          subtrees = subtrees + (Left -> context.actorOf(BinaryTreeNode.props(elemReq, false)))
          requester ! OperationFinished(id)
        }
      } else {
        removed = false
        requester ! OperationFinished(id)
      }
    }
  }

  def handleRemove(r: Remove): Unit = {
    r match {
      case Remove(requester, id, elemReq) =>
        if (elemReq > elem)
          if (subtrees.contains(Right)) subtrees(Right) ! r
          else requester ! OperationFinished(id)
        else if (elemReq < elem)
          if (subtrees.contains(Left)) subtrees(Left) ! r
          else requester ! OperationFinished(id)
        else {removed = true
          requester ! OperationFinished(id)}
    }
  }

  def handleCopy(a: ActorRef, c: CopyTo): Unit = {
    val sub = subtrees.values.toSet
    if (sub.isEmpty && removed) context.parent ! CopyFinished
    else {
      sub foreach (_ ! c)
      context.become(copying(sub, removed))
      if (!removed) a ! Insert(self, elem, elem)
      else self ! OperationFinished(elem)
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished => {
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        context.become(normal)
      }
      else context.become(copying(newExpected, insertConfirmed))
    }
    case OperationFinished(_) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        context.become(normal)
      } else context.become(copying(expected, true))
  }


}
