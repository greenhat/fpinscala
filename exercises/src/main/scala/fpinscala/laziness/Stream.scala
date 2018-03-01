package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    unfold((this, 0)) {
      case (Cons(h, t), taken) if taken < n => Some((h, (t(), taken + 1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (n == 0) cons(h(), t())
      else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h, t())
      case _ => None
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None.asInstanceOf[Option[A]])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(() => f(h()), t())
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case (Some(_), None) => true
      case _ => false
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((() => f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A] ,Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((() => (Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h, t)) => Some((() => (None, Some(h())), (Empty, t())))
      case (Cons(h, t), Empty) => Some((() => (Some(h()), None), (t(), Empty)))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    unfold[Stream[A], Option[Stream[A]]](Some(this)){
      case Some(ref@Cons(_, t)) => Some((() => ref, Some(t())))
      case Some(Empty) => Some((() => empty, None))
      case _ => None
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)(s => Some(() => s, s))

  def constant[A](a: A): Stream[A] = unfold(a)(s => Some(() => s, s))

  def from(n: Int): Stream[Int] = unfold(n)(s => Some((() => s, s + 1)))

  def fibs: Stream[Int] = unfold((0, 1))({
    case (prev1, prev2) => Some((() => prev1, (prev2, prev1 + prev2)))
  })

  def unfold[A, S](z: S)(f: S => Option[(() => A, S)]): Stream[A] = f(z).map {
    case (a, s) => cons(a(), unfold(s)(f))
  }.getOrElse(empty)
}