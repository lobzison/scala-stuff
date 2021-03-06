package untyped

import akka.actor._
import untyped.QueryExecutor._

object QueryExecutor {
  case class QueryRequest(id: Long, q: String)
  case class QueryReply(id: Long, r: String)
}
class QueryExecutor extends Actor {
  val es = ExternalSystem("DBMQKAFKAWHATEWER")

  override def receive: Receive = {
    case QueryRequest(id, q) =>
      es.openConnection()
      val results = es.executeQuery(q)
      sender ! QueryReply(id, results)
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    message foreach (self ! _)
  }

  override def postRestart(ex: Throwable):Unit = {
    es.closeConnection()
    super.postRestart(ex)
  }

  override def postStop(): Unit = {
    es.closeConnection()
    super.postStop()
  }
}
 