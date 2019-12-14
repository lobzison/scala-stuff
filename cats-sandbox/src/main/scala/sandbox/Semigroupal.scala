package sandbox
import cats.data.Validated

object SemigroupalApp {
    import cats._
    import cats.implicits._
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global 
    import scala.language.higherKinds
    import scala.util.Try

    val futurePair = Semigroupal[Future]. product(Future("Hello"), Future(123))
    Await.result(futurePair, 1.second)
    println(futurePair)

    case class Cat(
        name: String,
        yearOfBirth: Int,
        favoriteFoods: List[String]
    )

    val futureCat = (
        Future("Garfield"),
        Future(1978),
        Future(List("Lasagne"))
    ).mapN(Cat.apply)

    val res1 = Await.result(futureCat, 1.second)
    println(res1)

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = 
        for {
            xV <- x
            yV <- y
        } yield (xV, yV)

    case class User(name: String, age: Int) 
    
    def readName(m: Map[String, String]): Either[List[String], String] = 
        getValue("name")(m).flatMap(nonBlank(_))
    def readAge(m: Map[String, String]): Either[List[String], Int] = {
        for {
            ageStr <- getValue("age")(m)
            ageInt <- parseInt(ageStr)
            ageValid <- nonNegative(ageInt)
        } yield ageValid
    }
    def getValue(s: String)(d: Map[String, String]): Either[List[String], String] = {
        d.get(s).toRight(List(s"$s field is not specified"))
    }
    def parseInt(s: String): Either[List[String], Int] = {
        Either.fromTry(Try(s.toInt)).leftMap(e => List(s"Cant parse int $e"))
    }
    def nonBlank(s: String): Either[List[String], String] = 
        Either.cond(s.nonEmpty, s, List("Name is empty"))
    def nonNegative(i: Int): Either[List[String], Int] = 
        Either.cond(i >= 0, i, List("Numbero is negativo"))

    def readUser(m: Map[String, String]): Validated[List[String], User] = {
        val name = readName(m).toValidated
        val age = readAge(m).toValidated
        (name, age).mapN(User.apply)
    }
    
    
println(readUser(Map("name" -> "Dave", "age" -> "37")))
println(readUser(Map("age" -> "-1")))
    println(readName(Map("name" -> "dsa", "age" -> "332")))

}