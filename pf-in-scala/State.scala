
object State extends App {
  trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  }

  object RNG {
    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (n, newRng) = rng.nextInt
      (if (n < 0) -(n + 1) else n, newRng)
    }

    def double(rng: RNG): (Double, RNG) = {
      map(_.nextInt)(_ / (Int.MaxValue.toDouble + 1))(rng)
    }

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (n, newRng) = rng.nextInt
      val (dbl, newRngDbl) = double(rng)
      ((n, dbl), newRng)
    }

    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val (n, newRng) = rng.nextInt
      val (n2, newRng2) = newRng.nextInt
      ((n, n2), newRng2)
    }

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (d1, newRng1) = double(rng)
      val (d2, newRng2) = double(newRng1)
      val (d3, newRng3) = double(newRng2)
      ((d1,d2,d3), newRng3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      count match {
        case n if n <0 => (Nil, rng)
        case n => {val (h, newRng) = rng.nextInt
                   val (tail, endRng) = ints(n-1)(newRng)
          (h::tail, endRng)}
      }
    }

    def intsTail(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(n: Int, acc:(List[Int], RNG)): (List[Int], RNG) = {
        if (n >= count) acc
        else{
          val (h, endRng) = acc._2.nextInt
          go(n+1,(h::acc._1, endRng))
        }
      }
      go(0,(Nil, rng))
    }

    def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {val (an, aRand) = ra(rng)
              val (bn, bRand) = rb(aRand)
              (f(an,bn), bRand)}
    }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List[A]()))((h, end) => map2(h, end)(_::_))
    }

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
  }

  case class State[S,+A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      ???
    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object State {
    type Rand[A] = State[RNG, A]
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
  }


  val myList = RNG.ints(10)(new RNG.Simple(10L))
  println(myList)

  val myList2 = RNG.intsTail(10)(new RNG.Simple(10L))
  println(myList)
  val r = RNG.Simple(10L)
  println(RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(r)._1)
}