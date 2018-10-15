import scala.collection.mutable.ArrayBuffer

val test = "Lalal"

def add(x: Int, y: Int): Int= {
  x + y
}

add(3, 6)

def superSumm(args: Int*) = {
  args.fold(0)(_+_)
}

superSumm(6, 5,3, 3, 1)

def myProcedure: Unit = {
  println("asd")
}

lazy val myFile = scala.io.Source.fromFile("/Users/lobzison/Documents/test.py").mkString


def signum(num: Int): Int = {
  if (num == 0) 0 else {
    if (num < 0) -1 else 1
  }
}

val emptyBloack = {}

//val x, y: Int = 1


var x: Unit = ()
var y: Int = 1

x = y = 2

//“for (int i = 10; i >= 0; i--) System.out.println(i);”


"Hello".codePoints().toString()


val myNewArray = Array(1, 2, 3, 4, 4)
myNewArray.sortWith(_ > _)


val myNewBuffer = ArrayBuffer(1, 2, 3, 4)
myNewBuffer.sortWith(_ > _)

myNewArray.distinct

val myMap = Map("x" -> 1, "y" -> 3)
myMap("y")

myMap getOrElse("z", 0)

scala.collection.mutable.Map

val myGizmo = Map("thing" -> 15, "otherThing" -> 20, "somSuperThing" -> 1)

myGizmo.map(t => t._1 -> t._2 * 0.9)

import scala.io.Source

val source = Source.fromFile("/Users/lobzison/Documents/poug/autonomus_databases.py")

val linesIterator = source.getLines()

val mapped =
  for (line <- linesIterator;
     word <- line split " "
) yield (word, 1)

//mapped foreach {println(_)}

val res = mapped
  .toList
  .groupBy(_._1)
  .mapValues(_.map(_._2).sum)

import scala.collection.mutable.LinkedHashMap

val calendar = LinkedHashMap("Monday" -> java.util.Calendar.MONDAY,
                             "Thursday" -> java.util.Calendar.THURSDAY)

val text = java.lang.System.getProperties().toString()

val splitted = (text drop 1 dropRight 1 split ",").toList

val mappedVals =
splitted
  .map(_.trim.split("=").toList)
  .map {
    _ match {
      case k :: Nil => (k, None)
      case k :: v :: Nil => (k, Some(v))
    }
  }
  .toMap[String, Option[String]]

val longestKey = mappedVals.foldLeft(0)((end, pair) => Math.max(end, pair._1.length))

val padded =
  mappedVals.map(pair => pair._1 + " " * ((longestKey - pair._1.length) + 4) + "| " -> pair._2)

for {
  (key, value) <- padded
  realValue <-value
} {print(key)
   println(value.get)}


        //.foldLeft(Map())((mp, lst) => mp + (lst(0) -> lst(1))
//
//splitted
//
//splitted foreach {println(_)}
//
//" d ".trim.length()


def minmax(values:Array[Int]) = {
  values
    .foldLeft (Integer.MAX_VALUE, Integer.MIN_VALUE) {
      (end, x) => end match {
        case (a, b) if a > x && b < x => (x, x)
        case (a, b) if a > x => (x, b)
        case (a, b) if b < x => (a, x)
        case _ => end
      }
    }
}

def lteqgt(values: Array[Int], v: Int) = {
  values.
    foldLeft (Tuple3(0, 0, 0)) {
    (end, x) =>
      x match {
        case a if x < v => end.copy(_1 = end._1 + 1)
        case a if x == v => end.copy(_2 = end._2 + 1)
        case a if x > v => end.copy(_3 = end._3 + 1)
      }
  }
}


  println(minmax(Array(6,5,4,3)))
  println(lteqgt(Array(4,2,3,4,3,1,1,2,3,3,1,2,1,23),4))

println("Hello".zip("Worlda"))