import akka.util.ByteString

val a = ByteString(54, 54, 54, 124, 70, 124, 54, 48, 124, 53, 48)
println(a.utf8String)

val b = List(1,2)
val c = List(3)
val d = b ++ c

Nil ++ Nil

val e = Set[Int]()

e - 1