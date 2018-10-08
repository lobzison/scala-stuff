object Tree extends App {
  sealed trait Tree[A] {
    def fold[B](end: B)(f: (A, B) => B): B = {
      this match {
        case Leaf(v)    => f(v, end)
        case Node(l, r) => l.fold(r.fold(end)(f))(f)
      }
    }
  }
  final case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
  final case class Leaf[A](v: A) extends Tree[A]
//  sealed trait Tree[A] {
//    def fold[B](node: (B, B) => B, leaf: A => B): B
//  }
//  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
//    def fold[B](node: (B, B) => B, leaf: A => B): B =
//      node(left.fold(node, leaf), right.fold(node, leaf))
//  }
//  final case class Leaf[A](value: A) extends Tree[A] {
//    def fold[B](node: (B, B) => B, leaf: A => B): B =
//      leaf(value)
//  }

  val tree: Tree[String] =
    Node(Node(Leaf("To"), Leaf("iterate")),
         Node(Node(Leaf("is"), Leaf("human,")),
              Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

//  print(tree.fold("")(_+" "+_))
  //println(tree.fold[String]((a, b) => a + " " + b, str => str))

}
