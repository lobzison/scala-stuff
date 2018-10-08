object hw7 extends App {
  def addOption(one: Option[Int], other: Option[Int]): Option[Int] = {
    for {
      x <- one
      y <- other
    } yield x + y
  }

  def addOptionMap(one: Option[Int], other: Option[Int]): Option[Int] = {
    one.flatMap(x => other.map(_ + x))
  }

  println(addOptionMap(Some[Int](1), None))

  def addOption(one: Option[Int], other: Option[Int], other2: Option[Int]): Option[Int] = {
    for {
      x <- one
      y <- other
      z <- other2
    } yield x + y + z
  }

  def addOptionMap(one: Option[Int], other: Option[Int], other2: Option[Int]): Option[Int] = {
    one.flatMap(x => other.flatMap(y => other2.map(_ + x + y)))
  }

  def divide(divident: Int, divisor: Int): Option[Int] = {
    if (divisor == 0) None else Some(divident/divisor)
  }

  def divideOptions(divident: Option[Int], divisor: Option[Int]): Option[Int] = {
    for {
      x <- divident
      y <- divisor
      z <- divide(x, y)
    } yield z
  }

  import scala.util.Try
  val opt1 = Some(1)
  val opt2 = Some(2)
  val opt3 = Some(3)

  val seq1 = Seq(1)
  val seq2 = Seq(2)
  val seq3 = Seq(3)

  val try1 = Try(1)
  val try2 = Try(2)
  val try3 = Try(3)

  val optComb =
    for {
      a <- opt1
      b <- opt2
      c <- opt3
    } yield a + b + c

  val seqComb =
    for {
      a <- seq1
      b <- seq2
      c <- seq3
    } yield a + b + c

  val tryComb =
    for {
      a <- try1
      b <- try2
      c <- try3
    } yield a + b + c

  println(optComb)
  println(seqComb)
  println(tryComb)



}
