package sandbox

object Main  {
  case class Person(name: String)

  // trait Show[A] {
  //   def show(a: A): String
  // }
  // object Show {
  //   implicit val personShow: Show[Person] = new Show[Person] {
  //     def show(a: Person) = a.name
  //   }
  //   implicit val showInt: Show[Int] = new Show[Int] {
  //     def show(a: Int) = a.toString
  //   }
    
  // }
  // object ShowSyntax {
  //   implicit class ShowOps[A](a: A){
  //     def show(implicit s: Show[A]): String = s.show(a)
  //   }
  // }
  // // import ShowInstances._
  // import ShowSyntax._
  // val a = Person("asdqwe")
  // println(a.show)
  // println(1.show)
  import cats._
  import cats.implicits._
  val showInt: Show[Int] = Show[Int]
  val showString: Show[String] = Show[String]

  
  println(123.show)
  println("asdq".show)

  import java.util.Date

  // implicit val dateShow: Show[Date] = 
  //   new Show[Date] {
  //     def show(t: ju.Date): String = s"${t.getTime} ms since the epcoh."
  //   }
  
  val d = new java.util.Date()
  implicit val dateShow = Show.show[Date](x => s"${x.getTime} ms since the epcoh.")

  println(d.show)
  val l = List(1,2,3) === List(1,2,3)
  println(l)
  
  final case class Cat(name: String, age: Int, color: String)

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance{(a, b) => a == b}

  println(optionCat1 === optionCat2)
  println(optionCat1 === optionCat1)
}
