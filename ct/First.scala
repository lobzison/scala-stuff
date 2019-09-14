object First extends App {
    def id[A](a: A): A = a

    println(id(1))
    println(id("kek"))

    def comps[A, B, C](a: A => B, b: B => C): A => C = x => b(a(x))

    def square(x:Int): Int = x * x

    val test:Int => Int = square _ compose id
    println(test(2))

    val res = (1 to 10).toList
    val respectsIdentity = res.forall{x: Int =>
            val left: Int => Int = square _ compose id[Int]
            val right: Int => Int = id[Int] _ compose square
            left(x) == right(x)
        }
    println(respectsIdentity)
    // println("qwe")
    def memoize[A, B](f: A => B): A => B = {
        var cache = Map[A, B]()
        def mem(a: A): B = cache.get(a).getOrElse{
            val res = f(a)
            cache += (a -> res)
            res
        }
        mem(_)
    }

    def longBoi(x: Int): Int = {
        // Thread.sleep(1000)
        x * 2
    }

    val f = memoize(longBoi)
    (1 to 5).toList.foreach{x =>
        println(f(2))
    }

    trait Monoid[M] {
        def combin`e(m1: M, m2: M): M
        def empty: M
    }

    object Monoid {
        implicit val boolAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
            def combine(m1: Boolean, m2: Boolean): Boolean = m1 && m2
            def empty = true
        }
        implicit val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
            def combine(m1: Boolean, m2: Boolean): Boolean = m1 || m2
            def empty = false
        }
        implicit val moduloThree: Monoid[Int] = new Monoid[Int] {
            def combine(i1: Int, i2: Int): Int = (i1 + i2) % 3
            def empty = 3
        }
    }

}