import java.time.LocalDate

import scala.util.Random

object hw2 extends App {
  10.to(0, -1).foreach(println(_))

  def countdown(x: Int): Unit = {
    x.to(0, -1).foreach(println(_))
  }

  countdown(40)

  println("Hello".foldLeft(1L)(_ * _.toInt))

  def product(s: String): Long = {
    s.foldLeft(1L)(_ * _.toInt)
  }

  def product2(s: String): Long = {
    if (s.length == 0) 1L
    else s.head.toInt * product2(s.tail)
  }

  def toPow(base: Int, pow: Int): Int = {
    pow match{
      case 0               => 1
      case x if x < 0      => toPow(base, -pow)
      case x if x % 2 == 1 => base * toPow(base, pow - 1)
      case x               => toPow(base, x / 2) * toPow(base, x / 2)
  }
  }

  println(toPow(2, 8))

  implicit class DateInterpolator(val sc: StringContext) extends AnyVal {
    def date(args: Any*):LocalDate = {
      if (args.length != 3) throw new IllegalArgumentException("Needs three arguments")
      else {
        val ints = args map(_.toString.toInt)
        LocalDate.of(ints(0), ints(1), ints(2))
      }
    }
  }

  val year = 2015
  val month = 10
  val day = 15

  print(date"$year-$month-$day")

  def createRandomArray(n: Int): Array[Int] = {
    val res:Array[Int] = new Array[Int](n)
    0 until n foreach {x => res(x) = Random.nextInt()}
    res
  }

  println(createRandomArray(40).mkString(" "))


  def arraySwapper(a: Array[Int]): Unit = {
      0.until(a.length, 2).filter(_ + 1 < a.length).foreach {x =>
        val tmp = a(x+1)
        a(x+1) = a(x)
        a(x) = tmp
      }
  }

  val myArr = Array(1,2,3,4,5)
  arraySwapper(myArr)
  println(myArr.mkString(" "))

  def strangeStort(a: Array[Int]): Array[Int] = {
    (a filter {_ > 0}) ++ (a filter {_ <= 0})
  }

  println(strangeStort(Array(-1, -5, 2, 3, 1, 2, 1,1,1, -65, -3, -20000, -1)).mkString(" "))

  def avg(x: Array[Double]): Double = x.sum / x.length

  val myNewArray = Array(1, 2, 3, 4)
  myNewArray.sortWith(_ > _)



}