object SuperJSON extends App {

  sealed trait JValue {
    def printJList(jlist: JList): String = {
      jlist match {
        case LPair(head, t @ LPair(_, _)) => s"${head.print}, ${printJList(t)}"
        case LPair(head, LEnd())          => s"${head.print}"
      }
    }

    def printJDict(jdict: JDict): String = {
      jdict match {
        case DPair(k, v, d @ DPair(_, _, _)) =>
          s"'$k': ${v.print}, ${printJDict(d)}"
        case DPair(k, v, DEnd()) => s"'$k': ${v.print}"
      }
    }

    def print: String = {
      this match {
        case JString(value)     => s""" "$value" """
        case JDouble(value)     => value.toString
        case JBool(value)       => value.toString
        case l @ LPair(_, _)    => "[" + printJList(l) + "]"
        case d @ DPair(_, _, _) => "{" + printJDict(d) + "]"
      }
    }
  }
  final case class JString(value: String) extends JValue
  final case class JDouble(value: Double) extends JValue
  final case class JBool(value: Boolean) extends JValue

  sealed trait JList extends JValue
  final case class LEnd() extends JList
  final case class LPair(head: JValue, tail: JList) extends JList

  sealed trait JDict extends JValue
  final case class DEnd() extends JDict
  final case class DPair(key: String, value: JValue, tail: JDict) extends JDict

  println(LPair(JString("a string"),
                LPair(JDouble(1.0), LPair(JBool(true), LEnd()))).print)
  // res: String = ["a string", 1.0, true]

  println(
    DPair(
      "a",
      LPair(JDouble(1.0), LPair(JDouble(2.0), LPair(JDouble(3.0), LEnd()))),
      DPair(
        "b",
        LPair(JString("a"), LPair(JString("b"), LPair(JString("c"), LEnd()))),
        DPair(
          "c",
          DPair("doh",
                JBool(true),
                DPair("ray", JBool(false), DPair("me", JDouble(1.0), DEnd()))),
          DEnd()
        )
      )
    ).print)

}
