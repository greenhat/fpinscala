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

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

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

  def unit[S, B](b: B): State[S, B] =
    State[S, B](rng => (b, rng))

  def sequence[S, B](fs: List[State[S, B]]): State[S, List[B]] = fs match {
    case h :: t => h.flatMap(b => sequence(t).map(b +: _))
    case Nil => unit(Nil)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    sequence(
      inputs.map { input =>
        State[Machine, (Int, Int)] { machine =>
          input match {
            case Coin if machine.locked && machine.candies > 0 =>
              ((machine.candies, machine.coins + 1), Machine(locked = false, candies = machine.candies, coins = machine.coins + 1))
            case Turn if !machine.locked && machine.candies > 0 =>
              ((machine.candies - 1, machine.coins), Machine(locked = true, candies = machine.candies - 1, coins = machine.coins))
            case _ => ((machine.candies, machine.coins), machine)
          }
        }
      }).map(_.last)
  }
}
