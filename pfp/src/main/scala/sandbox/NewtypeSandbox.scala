package sandbox
import io.estatico.newtype.macros.newtype
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.api.Refined


object NewtypeSandbox {
    type EmailPred = MatchesRegex[W.`"""(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$)"""`.T]
    println("test")
    @newtype case class User(value: NonEmptyString) {
        def bf = s"!!!$value!!!"
    }
    @newtype case class Email(value: String Refined EmailPred)
    case class Test(value: String)
    println(User(" ").bf)
    println(Test("Benis"))
    println(Email("aas@a.c"))

}