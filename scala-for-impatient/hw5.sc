class BankAccount(initialBalance: Double) {
  private var balance = initialBalance
  def currentBalance = balance
  def deposit(amount: Double) = { balance += amount; balance }
  def withdraw(amount: Double) = { balance -= amount; balance }
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def deposit(amount: Double): Double = {
    withdraw(1)
    super.deposit(amount)
  }

  override def withdraw(amount: Double): Double = {
    withdraw(1)
    super.withdraw(amount)
  }
}

class SavingsAccount(initialBalance: Double)
  extends BankAccount(initialBalance) {
  private var actionsNum = 0

  override def deposit(amount: Double): Double = {
    actionsNum += 1
    super.deposit(amount)
  }

  override def withdraw(amount: Double): Double = {
    actionsNum += 1
    super.withdraw(amount)
  }

  def earnMonthlyInterest(interestRate: Double) = {
    if (actionsNum > 3) deposit(currentBalance * interestRate)
    actionsNum = 0
  }
}

// fuck java books

abstract class Item {
  def price: Int
  def description: String
}

class SimpleItem (override val price: Int ,override  val description: String) extends Item

class Bundle() extends Item{
  import scala.collection.mutable.ListBuffer
  private val items = ListBuffer[Item]()

  override def price: Int = items.foldLeft(0)((end, x) => end + x.price)
  override def description: String = items.foldRight("Items in bundle: ")((x, end) => end + x.description)
  def add(x: Item) = items += x
}

class Point(val x: Double, val y: Double)

class LabeledPoint(val label: String, override val x: Double, override val y: Double)
  extends Point(x, y)

new LabeledPoint("Black Thursday", 1929, 230.07)

abstract class Shape(){def centerPoint: Point}

class Rectangle(val leftBottom: Point, val rigthTop: Point)
  extends Shape {
  override def centerPoint: Point = new Point((leftBottom.x + rigthTop.x)/2, (leftBottom.y + rigthTop.y)/2)
}

class Circle(val center: Point, val radius: Double)
  extends Shape {
  override val centerPoint: Point = center
}

//not this java shit again

class Point2(private val xy: Long) extends AnyVal