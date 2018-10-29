
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

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (n, newRng) = rng.nextInt
      (if (n < 0) -(n + 1) else n, newRng)
    }

    def nonNegativeInt_():Rand[Int] = {
      rng => {
        val (n, rng2) = rng.nextInt
        (if (n < 0) -(n + 1) else n, rng2)
      }
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

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(x => map(rb)(y => f(x, y)))

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List[A]()))((h, end) => map2(h, end)(_::_))
    }

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
      rng => {
        val (n, rand) = f(rng)
        g(n)(rand)
      }
    }

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(x => unit(f(x)))


    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt){i:Int =>
        val mod = i % n
        if (i + (n-1) - mod >=0) unit(mod) else nonNegativeLessThan(n)
      }
    }
  }

  case class State[S,+A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      this.flatMap(x => State.unit(f(x)))

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      this.flatMap(x => sb.map(y => f(x, y)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(x => {
        val (v, state) = this.run(x)
        f(v).run(state)
      })
    }

  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object State {
    type Rand[A] = State[RNG, A]
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
    def unit[S, A](a:A):State[S, A] = State(s => (a, s))

    def sequence[S, A](xs: List[State[S,A]]):State[S, List[A]] = {
      xs.foldRight(State(s => (Nil:List[A], s)):State[S, List[A]])((x, end) => x.map2(end)(_::_))
    }
  }


  val myList = RNG.ints(10)(new RNG.Simple(10L))
  println(myList)

  val myList2 = RNG.intsTail(10)(new RNG.Simple(10L))
  println(myList)
  val r = RNG.Simple(10L)
  println(RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(r)._1)
}
