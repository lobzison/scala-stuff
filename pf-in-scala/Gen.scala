

import State.{State => State2}
import State.RNG


object Gen extends App{
  /*
  The library developed in this chapter goes through several iterations. This file is just the
  shell, which you can fill in and modify while working through the chapter.
  */

  // generate n lists, sum of original and reverse ==
  //sum of list with same element == element * list.length
  //sum of 0 == 0
  // sum of list :+ list of negatives == 0

  //max
  // max of list of same elements == element
  // max of list with 1 element == 1 element
  //
  import Gen._
  import Prop._

  case class Prop(run: (TestCases, RNG) => Result){
    def &&(p: Prop): Prop = Prop {
      (test, rng) => run(test,rng) match {
        case Passes => p.run(test,rng)
        case x => x
      }
    }

    def ||(p:Prop): Prop = Prop {
      (test, rng) => run(test, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(test, rng)
        case x => x
      }
    }

    def tag(msg: String): Prop = Prop {
      (test, rng) => run(test, rng) match {
        case Falsified(f, n) => Falsified(s"$msg\n$f", n)
        case x => x
      }
    }


  }

  object Prop {
    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
    type SuccessCount = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }
    final case object Passes extends Result {
      def isFalsified = false
    }
    final case class Falsified(failure: FailedCase,
                               success: SuccessCount) extends Result {
      override def isFalsified: Boolean = true
    }

  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
      n => g.listOfN(n)
    }
    
  }


  type TestCases = Int

  object Gen {
    def unit[A](a: => A): Gen[A] = Gen(State.State.unit(a))
    def boolean: Gen[Boolean] = Gen(State.State(RNG.boolean))
    def double: Gen[Double] = Gen(State.State(RNG.double))
    def listOfN[A](n: Int, g:Gen[A]): Gen[List[A]] =
      Gen(State.State.sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A])(g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  }

//  trait Gen[A] {
//    def map[A,B](f: A => B): Gen[B] = ???
//    def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//  }



  case class Gen[A](sample: State2[RNG, A]){
    def choose(start: Int, stopExclusive: Int): Gen[Int] =  Gen(State.State(RNG.nonNegativeInt).map(x => start + x % (stopExclusive - start)))

    def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(x => this.listOfN(x))
    def unsized: SGen[A] = SGen(_ => Gen(sample))
  }


  val a = Gen(State.State(RNG.double))
  val b = a.listOfN(10)
  println(b.sample)
}
