object hw4 extends App {
  import scala.io.Source

  val source = Source.fromFile("/Users/lobzison/Documents/poug/autonomus_databases.py")

  val linesIterator = source.getLines()

  val terribleMap = scala.collection.mutable.Map[String, Int]()

  for (line <- linesIterator;
    word <- line.split(" "))
    {(terribleMap get word) match {
      case Some(x) => terribleMap(word) += 1
      case None => terribleMap += word -> 1
    }
    }

  terribleMap foreach {println(_)}

  for (line <- linesIterator;
  word <- line split(" ")
  ) yield (word, 1)





}
