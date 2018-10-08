object hw6 extends App {
  //size
  //head and headOption - both retrive head, the second one retruns Option[A]
  // mkstring
  // exists
  // getOrElse
  // isDefined

  val animals = Seq("cat", "dog", "penguin")
  println(animals)

  println("mouse" +: animals :+ "tyrannosaurus")
  println((2 +: animals).getClass())

  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])

  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)

  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  def directedMoreThan(num: Int): Seq[Director] = {
    directors.filter(x => x.films.length > num)
  }

  def bornBefore(year: Int): Seq[Director] = {
    directors.filter(x => x.yearOfBirth < year)
  }

  def yadayada(year: Int, num: Int): Seq[Director] = {
    directors.filter(x => x.films.length > num && x.yearOfBirth < year)
  }

  def mySort(ascending: Boolean): Seq[Director] = {
    if (ascending)
      directors.sortWith((x, y) => x.yearOfBirth < y.yearOfBirth)
    else
      directors.sortWith((x, y) => x.yearOfBirth > y.yearOfBirth)
  }

  println(directedMoreThan(4))
  println(bornBefore(1980))
  println(mySort(false))

  directors.foreach(_ => println("huihuihui"))
  val nolanFilms = nolan.films
  println(nolanFilms)

  val allFilms = directors.flatMap(x => x.films)
  println(allFilms)

  val earliestFilmBy = mcTiernan.films.foldLeft(Int.MaxValue)((x: Int, y: Film) => math.min(x, y.yearOfRelease))
  println(earliestFilmBy)

  val sortByRating = directors.flatMap(_.films).sortWith((x, y) => x.imdbRating < y.imdbRating)
  println(sortByRating)

  val allFilmsLen = directors.flatMap(_.films).length
  val avgScore = directors.flatMap(_.films).foldLeft(0.0)((x, y) => x + y.imdbRating) / allFilmsLen
  println(avgScore)

  directors.foreach(x => x.films.foreach(y => println(s"Tonight only! ${y.name} by ${x.lastName}!")))

  val oldest = directors.flatMap(_.films).foldLeft(Film("none", 3000, 0))((x, y) => if (x.yearOfRelease < y.yearOfRelease) x else y)
  println(oldest)

  def myMininum(s: Seq[Int]): Int = {
    s.foldLeft(Int.MinValue)(math.min(_,_))
  }

  def myuUnique(s: Seq[Int], end:Seq[Int] = Seq[Int]()): Seq[Int] = {
    s match {
      case Seq() => end
      case Seq(h, tl@_*) => if (!end.contains(h)) myuUnique(tl, h +: end) else myuUnique(tl, end)
    }
  }

  def myUnique2(s: Seq[Int]): Seq[Int] = {
  s.foldLeft(Seq[Int]())((x, y) => if (!x.contains(y)) y +: x else x )
  }

  val test = Seq(1, 1, 2, 4, 3, 4)
  println(myuUnique(test))
  println(myUnique2(test))

  def reverse[A](s: Seq[A]): Seq[A] = {
    s.foldLeft(Seq[A]())((l, v) => v +: l)
  }
  println(reverse(test))

  def myMap[A,B](s: Seq[A], func: A => B):Seq[B] = {
    s.foldRight(Seq[B]())((v, l) => func(v) +: l)
  }

  for {
    film <- nolan.films
  } println(film.name)

  for {
    director <- directors
    film <- director.films
  } println(film.name)

  for {
    director <- directors
    film <- director.films.sortWith((x, y) => x.imdbRating < y.imdbRating)
  } println(film.name)

  for {
    director <- directors
    film <- director.films
  } println(s"Hui zalupa ${film.name} by ${director.lastName}")
}
