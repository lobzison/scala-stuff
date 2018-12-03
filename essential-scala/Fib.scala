object Fib extends App {
  def fib(n: Int): Long = {
    // binets formula
    val sqrtFive = math.sqrt(5)
    val Phi  = (sqrtFive + 1) / 2.0
    val res = math.pow(Phi, n) / sqrtFive
    math.round(res)
  }
}
