object SuperCalc extends App {
  sealed trait Expression {
    def eval: Coputation = {
      this match {
        case Addition(l, r) =>
          l.eval match {
            case Fail(msg) => Fail(msg)
            case Success(r1) =>
              r.eval match {
                case Fail(msg)   => Fail(msg)
                case Success(r2) => Success(r1 + r2)
              }
          }
        case Substraction(l, r) =>
          l.eval match {
            case Fail(msg) => Fail(msg)
            case Success(r1) =>
              r.eval match {
                case Fail(msg)   => Fail(msg)
                case Success(r2) => Success(r1 - r2)
              }
          }
        case Number(value) => Success(value)
        case Division(l, r) =>
          l.eval match {
            case Fail(msg) => Fail(msg)
            case Success(r1) =>
              r.eval match {
                case Fail(msg)   => Fail(msg)
                case Success(0)  => Fail("Division by zero")
                case Success(r2) => Success(r1 / r2)
              }
          }
        case SquareRoot(exp) =>
          exp.eval match {
            case Fail(msg) => Fail(msg)
            case Success(r1) =>
              if (r1 < 0) Fail("Negative root") else Success(math.sqrt(r1))
          }
      }
    }
  }
  final case class Addition(left: Expression, right: Expression)
      extends Expression
  final case class Substraction(left: Expression, right: Expression)
      extends Expression
  final case class Number(value: Double) extends Expression
  final case class Division(left: Expression, right: Expression)
      extends Expression
  final case class SquareRoot(exp: Expression) extends Expression

  sealed trait Coputation
  final case class Fail(msg: String) extends Coputation
  final case class Success(value: Double) extends Coputation
}
