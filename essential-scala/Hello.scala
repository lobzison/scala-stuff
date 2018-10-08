object Hello extends App {
  object Test3 {
    val name: String = "Lobz"
    def hello(other: String) =
      s"$name say hello to $name"
  }

  println(Test3.hello("Lobz"))
  object Test4 {
    val simpleFiled = {
      println("simplefield")
      42
    }

    def simpleMethod = {
      println("simpleMethod")
      42
    }
  }

  object cat1 {
    val name = "Oswald"
    val color = "Black"
    val food = "Milk"
  }
  object cat2 {
    val name = "Henderson"
    val color = "Ginger"
    val food = "Chips"
  }
  object cat3 {
    val name = "Quentin"
    val color = "Tabby and white"
    val food = "Curry"
  }

  object calc {
    def square(num: Double) = num * num
    def cube(num: Double) = square(num) * num
  }
  object calc2 {
    def square(num: Double) = num * num
    def cube(num: Double) = square(num) * num
    def square(num: Int) = num * num
    def cube(num: Int) = square(num) * num
  }

  // prints b a c a a
  // String 3c31

  object person {
    val firstName = "name"
    val secondName = "second"
  }

  object alien {
    def greet(other: person.type) = {
      val name = other.firstName
      println(s"$name hello!")
    }
  }

  object calc3 {
    def square(num: Double): Double = num * num
  }

  assert(calc3.square(3.0) == 9.0)
  assert(calc3.square(-3.0) == 9.0)
  assert(calc3.square(1.0) == 1.0)

  def testif(num1: Int, num2: Int): String = {
    if (num1 < num2) 42.toString
    else "test"
  }

  print(testif(3, 1).getClass)

  //String predator
  //Any 2001
  //Any ()
  //When branch expression types are no match, scala chooses closes common ancestor, in this case - Any

  class Cat(val name: String, val colour: String, val food: String)
  val c1 = new Cat(name = "Oswald", colour = "Black", food = "Milk")
  val c2 = new Cat(name = "Henderson", colour = "Ginger", food = "Chips")
  val c3 = new Cat(name = "Quentin", colour = "Tabby and white", food = "Curry")

  object ChipChop {
    def willServe(cat: Cat): Boolean = {
      cat.food == "Chips"
    }
  }

  assert(ChipChop.willServe(c1) == false)
  assert(ChipChop.willServe(c2) == true)

  class Director(val firstName: String,
                 val lastName: String,
                 val yearOfBirth: Int) {
    def name(): String = {
      s"$firstName $lastName"
    }

    override def toString: String =
      s"Director $firstName, $lastName, $yearOfBirth"
  }

  class Film(val name: String,
             val yearOfRelease: Int,
             val imdbRating: Double,
             val director: Director) {
    def directorsAge(): Int = {
      yearOfRelease - director.yearOfBirth
    }

    def isDirectedBy(other_director: Director): Boolean = {
      other_director == director
    }

    def copy(name: String = this.name,
             yearOfRelease: Int = this.yearOfRelease,
             imdbRating: Double = this.imdbRating,
             director: Director = this.director): Film = {
      new Film(name = name,
               yearOfRelease = yearOfRelease,
               imdbRating = imdbRating,
               director = director)
    }

    override def toString: String =
      s"Film $name, $yearOfRelease, $imdbRating, $director"
  }

  val d1 =
    new Director(firstName = "ivan", lastName = "ivanov", yearOfBirth = 1900)
  val d2 = new Director(firstName = "stepan",
                        lastName = "stepanovich",
                        yearOfBirth = 1885)
  val f1 = new Film(name = "Ivanovshina",
                    yearOfRelease = 1920,
                    imdbRating = 5,
                    director = d1)
  val f2 = new Film(name = "Ivanovshina2",
                    yearOfRelease = 1899,
                    imdbRating = 5,
                    director = d1)
  assert(f1.directorsAge() == 20)
  assert(f2.directorsAge() == -1)
  assert(!f2.isDirectedBy(d2))
  assert(f2.isDirectedBy(d1))

  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "Some Body", 1990)

  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)

  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober =
    new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
  val thomasCrownAffair =
    new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  println()
  println(eastwood.yearOfBirth)
  println(dieHard.director.name)
  println(invictus.isDirectedBy(nolan))
  println(highPlainsDrifter.copy(name = "L'homme des hautes plaines"))
  println(
    thomasCrownAffair.copy(yearOfRelease = 1968,
                           director = new Director("Norman", "Jewison", 1926)))
  // returns Film("The Thomas Crown Affair", 1926, /* etc */)
  println(inception.copy().copy().copy())
  // returns a new copy of `inception`

  class Adder(amount: Int) {
    def add(in: Int) = in + amount
  }

  class Counter(val count: Int) {

    def inc(step: Int = 1): Counter = {
      new Counter(count + step)
    }
    def dec(step: Int = 1): Counter = {
      new Counter(count - step)
    }

    def adjust(adder: Adder): Counter = {
      new Counter(adder.add(count))
    }

  }

  println(new Counter(10).inc().dec().inc().inc().count)

}
