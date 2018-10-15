class Counter {
  private var value = 0 // You must initialize the field
  def increment() = if (value <= Integer.MAX_VALUE) value += 1 // Methods are public by default
  def current() = value
}

class BankAccount {
  private var balanceLocal = 0
  def deposit(value: Int) = balanceLocal += value
  def winthraw(value: Int): Int = {
    var res = 0
    if (balanceLocal > value) {
      balanceLocal -= value
      res = value
    }
    return res
  }
  def balance = balanceLocal
}

val ba = new BankAccount
ba.balance
ba.deposit(100)
ba.winthraw(50)
ba.winthraw(70)
ba.balance

class Time(private[this] val hh: Int,
           private[this] val mi: Int) {
  private val minutesSinceMidnight = hh * 24 + mi
  def hours = hh
  def minutes = mi
  def before (other: Time): Boolean = {
    this.minutesSinceMidnight < other.minutesSinceMidnight
  }
}

val t1 = new Time(16, 20)
val t2 = new Time(16, 19)

t2.before(t1)
t1.before(t2)

import scala.beans.BeanProperty

class Student(@BeanProperty var name: String,
              @BeanProperty var id: Long)

val st = new Student("Bitch", 12)
st.setId(14)
st.getId

class Person(var age: Int) {
  if (age < 0) age = 0
}

val p = new Person(10)
val p2 = new Person(-1)
p.age
p2.age

class Person2(fullName: String) {
  val firstName = fullName.split(" ")(0)
  val lastName = fullName.split(" ")(1)
}

val p3 = new Person2("Bitch Lasagna")

p3.firstName
p3.lastName

class Car(val manufacturer: String,
          val modelName: String,
          val modelYear: Int = -1,
          var licencePlate: String = "")



