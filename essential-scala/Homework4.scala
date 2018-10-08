object Homework4 extends App {
  sealed trait IntList {
    def fold[A](end: A, f: (Int, A) => A): A = {
      this match {
        case End()        => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }
    }

    def sum = {
      this.fold[Int](0, (x, y) => x + y)
    }
    def length = {
      this.fold[Int](0, (_, y) => y + 1)
    }
    def product = {
      this.fold[Int](1, (x, y) => x * y)
    }

    def double = {
      this.fold[IntList](End(), (x: Int, y: IntList) => Pair(x * 2, y))
    }
  }
  final case class End() extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End())))
  println(example.sum)
  println(example.length)
  println(example.product)
  print(example.double)

}
