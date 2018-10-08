object hw9 extends App {
  val subjects = List("Noel", "The cat", "The dog")
  val verbs = List("wrote", "chased", "slept on")
  val objects = List("the book", "the ball", "the bed")

  val comb = for {
    subj <- subjects
    verb <- verbs
    obj <- objects
  } yield (subj, verb, obj)

  println(comb)

  def getVerb(subj: String): List[String] = {
    subj match {
      case "Noel" => verbs
      case "The cat" => List("meowed at", "chased", "slept on")
      case "The dog" => List("barked at", "chased", "slept on")
    }
  }

  def getObj(verb: String): List[String] = {
    verb match {
      case "wrote" => List("the book", "the letter", "the code")
      case "chased" => List("the ball", "the dog", "the cat")
      case "slept on" => List("the bed", "the mat", "the train")
      case "meowed at" => List("Noel", "the doctor", "the food cupboard")
      case "barked at" => List("the postman", "the car", "the cat")
    }
  }

  val comb2 = for {
    subj <- subjects
    verb <- getVerb(subj)
    obj <- getObj(verb)
  } yield (subj, verb, obj)

  println(comb2)

  import math.abs

  //  assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
  //  assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))
  //

  final case class Rational(numerator: Int, denominator: Int)

  def rationalOrder(r1: Rational, r2: Rational): Boolean = {
    r1.numerator * r2.denominator < r2.numerator * r1.denominator
  }

  implicit val absOrdered = Ordering.fromLessThan(rationalOrder)

  println(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted)

  assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
    List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))


  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  case object Order {
    implicit val ordering = Ordering.fromLessThan((x: Order, y: Order) => x.totalPrice < y.totalPrice)
  }

  object byUnits {
    implicit val ordering = Ordering.fromLessThan((x: Order, y: Order) => x.units < y.units)
  }

  object byPrice {
    implicit val ordering = Ordering.fromLessThan((x: Order, y: Order) => x.unitPrice < y.unitPrice)
  }

  trait Equal[A] {
    def equal(one:A, other: A): Boolean
  }

  case class Person(name: String, email: String)

  object byName extends Equal[Person] {
    override def equal(one: Person, other: Person): Boolean = one.name == other.name
  }

  object full extends Equal[Person] {
    override def equal(one: Person, other: Person): Boolean = one == other
  }

}
