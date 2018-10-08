object JSON2 extends App {

  sealed trait JsValue {
    def stringify: String
  }

  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    def stringify = values
      .map { case (name, value) => "\"" + name + "\":" + value.stringify }
      .mkString("{", ",", "}")
  }

  final case class JsString(value: String) extends JsValue {
    def stringify = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
  }

  //type class
  trait JsWriter[A] {
    def write(value: A): JsValue
  }

  implicit class JsUtil[A](a: A) {
    def toJson(implicit writer: JsWriter[A]): JsValue = {
      writer.write(a)
    }
  }

  import java.util.Date

  // some old shit
  sealed trait Visitor {
    def id: String

    def createdAt: Date

    def age: Long = new Date().getTime() - createdAt.getTime()
  }

  final case class Anonymous(
                              val id: String,
                              val createdAt: Date = new Date()
                            ) extends Visitor

  final case class User(
                         val id: String,
                         val email: String,
                         val createdAt: Date = new Date()
                       ) extends Visitor

  // some old shit end

  implicit object AnonymousJsWriter extends JsWriter[Anonymous] {
    override def write(value: Anonymous): JsValue = {
      JsObject(Map(
        "id" -> JsString(value.id),
        "created at" -> JsString(value.createdAt.toString)))
    }
  }

  implicit object UserJsWriter extends JsWriter[User] {
    override def write(value: User): JsValue = {
      JsObject(Map(
        "id" -> JsString(value.id),
        "created at" -> JsString(value.createdAt.toString),
        "Email" -> JsString(value.email)))
    }
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor) = value match {
      case anon: Anonymous => anon.toJson
      case user: User => user.toJson
    }
  }

  val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))
  println(visitors map (x => x.toJson))
}
