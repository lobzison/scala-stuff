object hw10 extends App {

  case class Person(name: String, email: String)

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object Equal {
    def apply[A](implicit instance: Equal[A]): Equal[A] =
      instance
  }

  object EmailEqualObject {

    implicit object EmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email
    }

  }

  object NameEmailObject {

    implicit object NameEmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email && v1.name == v2.name
    }

  }

  import EmailEqualObject._

  println(Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noael", "noel@example.com")))

  object IntImplicits {

    implicit class IntEnrich(num: Int) {
      def eiBoss = {
        times(_ => println("EIBOSS"))
      }

      def times(func: Int => Unit): Unit = {
        0 until num foreach (x => func(x))
      }
    }

  }

  import IntImplicits._

  3.eiBoss
  -1.eiBoss
  100.eiBoss

  object EqualImplicits {
    implicit class EqualEnrich(str: String) {
      def ===(otherStr: String): Boolean = {
        str.toLowerCase == otherStr.toLowerCase
      }
    }
  }

  import EqualImplicits._
  println("asdasd" === "aSdAsd")
}
