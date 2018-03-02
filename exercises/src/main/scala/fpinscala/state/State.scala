package fpinscala.state


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
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    map(int)(math.abs)(rng)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1.0))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb)(b => unit(f(a, b)))
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case h :: t => map(sequence(t))(l => h(rng)._1 +: l)(rng)
        case Nil => unit(Nil)(rng)
      }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      flatMap(nonNegativeInt) { a =>
        val mod = a % n
        if (a + (n-1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThan(n)
      }(rng)
    }
}

case class State[S,+A](run: S => (A, S)) {

  def unit[B](b: B): State[S, B] =
    State[S, B](rng => (b, rng))

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a,b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B] { s =>
      val (a, s2) = this.run(s)
      f(a).run(s2)
    }

  def sequence[B](fs: List[State[S, B]]): State[S, List[B]] =
    State[S, List[B]] { s =>
      fs match {
        case h :: t => sequence(t).map(l => h.run(s)._1 +: l).run(s)
        case Nil => unit(Nil).run(s)
      }
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State[Machine, (Int, Int)] { inputMachine =>
      def loop(resState: ((Int, Int), Machine), rest: List[Input]): ((Int, Int), Machine) = {
        val machine = resState._2
        val candiesLeft = resState._1._1
        val coinsLeft = resState._1._1
        rest match {
          case input :: t if machine.candies > 0 => loop(
            input match {
              case Coin if machine.locked =>
                ((candiesLeft, coinsLeft + 1), Machine(locked = false, candies = machine.candies, coins = machine.coins + 1))
            }, t)
          case _ => resState
        }
      }
      loop(((inputMachine.candies, inputMachine.coins), inputMachine), inputs)
    }
}
