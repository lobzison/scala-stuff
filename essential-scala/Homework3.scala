import scala.annotation.tailrec

object Homework3 extends App {

  sealed trait Calculation
  final case class Result(number: Int) extends Calculation
  final case class Fail(message: String) extends Calculation

  case object Calculator {
    def +(calculation: Calculation, number: Int) = {
      calculation match {
        case Result(num) => Result(num + number)
        case Fail(msg)   => Fail(msg)
      }
    }
    def -(calculation: Calculation, number: Int) = {
      calculation match {
        case Result(num) => Result(num - number)
        case Fail(msg)   => Fail(msg)
      }
    }

    def /(calculation: Calculation, number: Int): Calculation = {
      (calculation, number) match {
        case (Fail(msg), _)      => Fail(msg)
        case (Result(_), 0)      => Fail("Division by zero")
        case (Result(num), num2) => Result(num / num2)
      }
    }
  }

  assert(Calculator.+(Result(1), 1) == Result(2))
  assert(Calculator.-(Result(1), 1) == Result(0))
  assert(Calculator.+(Fail("Badness"), 1) == Fail("Badness"))

  assert(Calculator./(Result(4), 2) == Result(2))
  assert(Calculator./(Result(4), 0) == Fail("Division by zero"))
  assert(Calculator./(Fail("Badness"), 0) == Fail("Badness"))

  //in email service, since it requires data from outside of class

  sealed trait IntList {
    def length: Int = {
      this match {
        case End              => 0
        case Pair(head, tail) => 1 + tail.length
      }
    }
    def product: Int = {
      this match {
        case End          => 1
        case Pair(hd, tl) => hd * tl.product
      }
    }

    def double: IntList = {
      this match {
        case End          => End
        case Pair(hd, tl) => Pair(hd * 2, tl.double)
      }
    }
  }
  final case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  sealed trait Tree {
    def sum: Int = {
      this match {
        case Node(left, right) => left.sum + right.sum
        case Leaf(num)         => num
      }
    }
    def double: Tree = {
      this match {
        case Node(left, right) => Node(left.double, right.double)
        case Leaf(num)         => Leaf(num * 2)
      }
    }
  }
  final case class Node(left: Tree, right: Tree) extends Tree
  final case class Leaf(value: Int) extends Tree

}
