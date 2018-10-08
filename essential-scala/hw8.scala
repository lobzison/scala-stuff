object hw8 extends App{
  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")

  val ages = Map(
    "Alice"   -> 20,
    "Bob"     -> 30,
    "Charlie" -> 50,
    "Derek"   -> 40,
    "Edith"   -> 10,
    "Fred"    -> 60)

  val favoriteColors = Map(
    "Bob"     -> "green",
    "Derek"   -> "magenta",
    "Fred"    -> "yellow")

  val favoriteLolcats = Map(
    "Alice"   -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith"   -> "Cloud Cat")

  def favoriteColour(name: String):String = {
    favoriteColors getOrElse(name, "beige")
  }

  println(favoriteColour("Bob"))
  println(favoriteColour("hui"))

  def printColors: Unit = {
    favoriteColors foreach (pair => println(s"${pair._1}'s favorite color is ${pair._2}"))
  }

  printColors

  def lookup[A](name: String, map:Map[String, A]):Option[A] = {
    map get name
  }

  println(lookup("Bob", favoriteColors))
  println(lookup("Bob", ages))

  println(favoriteColors get ages.foldLeft(("none", 0))((x, y) => if (y._2 > x._2) y else x)._1)






}
