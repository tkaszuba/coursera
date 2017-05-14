/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

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


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
  def newRoot:ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
  
  var root = createRoot
  
  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
    case operation: Operation => root ! operation

    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))  
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive =  {
    case operation: Operation => pendingQueue.enqueue(operation)

    case CopyFinished => {
      root ! PoisonPill
      val newRoot = createRoot
      root = newRoot

      pendingQueue.map(root ! _)
      pendingQueue = Queue.empty

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

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = { 
    case Insert(requester, id, elemToInsert) => {

      if (elem != elemToInsert || (elem==elemToInsert && removed)) {

        val child = childToVisit(elemToInsert)

        if (subtrees.contains(child)) {
          subtrees(child) ! Insert(requester, id, elemToInsert)
        }
        else {
          subtrees += (child -> context.actorOf(BinaryTreeNode.props(elemToInsert, false)))
          requester ! OperationFinished(id)
        }

      }
      else {
        requester ! OperationFinished(id)
      }

    }
    case Remove(requester, id, elemToRemove) => {

      if (elem != elemToRemove || (elem==elemToRemove && removed)) {

        val child = childToVisit(elemToRemove)

        if (subtrees.contains(child)) {
          subtrees(child) ! Remove(requester, id, elemToRemove)
        }
        else {
          requester ! OperationFinished(id)
        }
      }
      else {
        removed = true
        requester ! OperationFinished(id)
      }
    }
    case Contains(requester, id, elemToFind) => {

      if (elem != elemToFind || (elem==elemToFind && removed)) {

        val child = childToVisit(elemToFind)

        if (subtrees.contains(child)) {
          subtrees(child) ! Contains(requester, id, elemToFind)
        }
        else {
          requester ! ContainsResult(id, false)
        }

      }
      else {
        requester ! ContainsResult(id, true)
      }
    }
     case CopyTo(newRoot) => {
      if (!removed){
        newRoot ! Insert(self, 0, elem)
      }
      if (removed && subtrees.isEmpty){
        sender ! CopyFinished
      }
      else{
        context.become(copying(subtrees.values.toSet, insertConfirmed = removed, sender))
      }
      subtrees.values foreach (_ ! CopyTo(newRoot))
     }
  }
  def childToVisit(elemToFind: Int): Position = {
    if (elemToFind > elem) Right
    else Left
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean, originator: ActorRef): Receive = {
    case OperationFinished(_) =>
      if (expected.isEmpty) {
        originator ! CopyFinished
        context.become(normal)
      } else {
        context.become(copying(expected, insertConfirmed = true, originator))
      }
    case CopyFinished =>
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        originator ! CopyFinished
        context.become(normal)
      } else {
        context.become(copying(newExpected, insertConfirmed, originator))
      }
  }

}
