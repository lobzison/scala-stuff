class Employee(val name: String = "John Q. Public", var salary: Double = 0.0)


abstract class UnitConversions{
  var unit: Option[String] = None
  def convert(amount: Double) = {
    unit match {
      case Some("inches") => amount * 2.3
      case Some("gallons") => amount * 4
      case Some("miles") => amount * 5
      case _ => 0.0
    }
  }
}

object Conversions {
  def inchesToCantimeters(inches: Double): Double = inches * 2.3
  def gallonsToLiters(gallons: Double): Double = gallons * 4
  def milesToKilometers(miles: Double): Double =  miles * 5
}

object InchesToCantimeters extends UnitConversions {
  unit = Some("inches")
}

object GallonsToLiters extends UnitConversions {
  unit = Some("gallons")
}

object MilesToKillometers extends UnitConversions {
  unit = Some("miles")
}

object Origin extends java.awt.Point {
  //mutable
}

class Point(val x: Int, val y: Int)
object Point {
  def apply(x: Int,y: Int): Point = new Point(x, y)
}
Point(3, 4)


object Suits extends Enumeration {
  val ♣, ♦, ♥, ♠ = Value
}

Suits.values.foreach(println)

def isRed(card: Suits.Value): Boolean = {
  card == Suits.♦ || card == Suits.♥
}

object RubicsCibe extends Enumeration {
  val Red = Value(0xff0000)
  //etc
}
