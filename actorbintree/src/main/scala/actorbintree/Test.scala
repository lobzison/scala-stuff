package actorbintree

import akka.actor.{Actor, Props}
import akka.event.LoggingReceive

class Test extends Actor{
  import BinaryTreeSet._
  val tree = context.actorOf(Props[BinaryTreeSet])

  override def receive: Receive = LoggingReceive{case x => println(x)}
  tree ! Insert(self, id=100, 1)
  tree ! Contains(self, id=50, 2)
  tree ! Remove(self, id=10, 1)

  context.stop(self)

}
