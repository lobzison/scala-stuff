object Homework2 extends App {
  class Person(val name: String, val lastName: String)

  object Person {
    def apply(fullName: String) = {
      val parts = fullName.split(" ")
      new Person(parts(0), parts(1))
    }
  }

  val mike = Person("Mike Tyson")
  assert(mike.name == "Mike")
  assert(mike.lastName == "Tyson")
  println(mike.name)

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

  object Director {
    def apply(firstName: String, lastName: String, yearOfBirth: Int) = {
      new Director(firstName, lastName, yearOfBirth)
    }
    def older(one: Director, other: Director) = {
      one.yearOfBirth < other.yearOfBirth
    }
  }

  object Film {
    def apply(name: String,
              yearOfRelease: Int,
              imdbRating: Double,
              director: Director): Film =
      new Film(name, yearOfRelease, imdbRating, director)

    def HighestRating(one: Film, other: Film): Film =
      if (one.imdbRating > other.imdbRating) one else other

    def oldestDirectorAtTheTime(one: Film, other: Film): Film =
      if (one.directorsAge() > other.directorsAge()) one else other
  }

  //type type value value value

  sealed trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

//  sealed trait Rectangular extends Shape{
//    val sides = 4
//    def width: Double
//    def height: Double
//    def perimeter = width * 2 + height * 2
//    def area = width * height
//  }
//
//  case class Circle(radius: Double, color: Color) extends Shape {
//    val sides = 1
//    def perimeter = 2 * math.Pi * radius
//    def area = math.pow(radius, 2.0) * math.Pi
//  }
//
//  case class Rectange(width: Double, height: Double, color: Color) extends Rectangular
//
//  case class Square(side: Double, color: Color) extends Rectangular{
//    val width = side
//    val height = side
//  }

//  object Draw {
//    def apply(shape: Shape): Unit = {
//      shape match {
//        case Circle(radius, color) => println(s"A circle of radius: $radius of color: ${Draw(color)}")
//        case Rectange(w, h, color) => println(s"A rectangular with width: $w, height: $h of color: ${Draw(color)}")
//        case Square(side, color) => println(s"A square with side: $side of color: ${Draw(color)}")
//      }
//    }
//
//    def apply(color: Color): String = {
//      color match {
//        case Yellow => "Yellow"
//        case Red => "Red"
//        case Pink => "Pink"
//        case color => color.desribe
//      }
//    }
//  }
//
//  sealed trait Color {
//    def r: Int
//    def g: Int
//    def b: Int
//    def desribe = if (r+g+b > (255*3)/2) "light" else "dark"
//  }
//
//  final case object Yellow extends Color {
//    val r = 255
//    val g = 255
//    val b = 0
//  }
//  final case object Red extends Color {
//    val r = 255
//    val g = 0
//    val b = 0
//  }
//
//  final case object Pink extends Color {
//    val r = 255
//    val g = 125
//    val b = 125
//  }

//  sealed case class CustomColor(r: Int, g: Int, b: Int) extends Color

  case object divide {
    def apply(enumerator: Int, denominator: Int): DivisionResult =
      denominator match {
        case 0 => Infinite
        case _ => Finite(enumerator / denominator)
      }
  }

  sealed trait DivisionResult

  final case class Finite(num: Int) extends DivisionResult
  final case object Infinite extends DivisionResult

  println(divide(1, 2))
  println(divide(1, 0))

  ///
//  sealed trait TrafficLightColor{
//    def next:TrafficLightColor
//  }
//
//  final case object Red extends TrafficLightColor {
//    def next: TrafficLightColor = Green
//  }
//  final case object Green extends TrafficLightColor {
//    def next:TrafficLightColor = Yellow
//  }
//  final case object Yellow extends TrafficLightColor{
//    def next:TrafficLightColor = Red
//  }

  sealed trait TrafficLightColor {
    def next: TrafficLightColor = {
      this match {
        case Red    => Green
        case Green  => Yellow
        case Yellow => Red
      }
    }
  }
  final case object Red extends TrafficLightColor
  final case object Green extends TrafficLightColor
  final case object Yellow extends TrafficLightColor

  sealed trait Calculation
  final case class Result(number: Int) extends Calculation
  final case class Fail(message: String) extends Calculation

}
