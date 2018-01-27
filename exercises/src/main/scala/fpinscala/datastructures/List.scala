package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, z) => Cons(a, z))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (l == Nil) Nil
    else if (n == 0) l
    else drop(List.tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

  def initDropLast[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      def loop(rest: List[A], xs: List[A]): List[A] = rest match {
        case Nil => Nil
        case Cons(_, Nil) => xs
        case Cons(head, tail) => loop(tail, List.append(xs, List(head)))
      }
      loop(t, List(h))
  }

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(List.reverse(l), z)((b, a) => f(a, b))

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = foldLeft(l, 0)((z, _) =>  z + 1)

  def lengthViaFoldRight[A](l: List[A]): Int = foldRight(l, 0)((_, z) =>  z + 1)

  def reverse[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((a, z) => List.append(z, Cons(a, Nil)))

  def reverseViaFoldLeft[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((z, a) => Cons(a, z))

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(List.reverse(l), z)((a, z) => f(z, a))

  def flatten[A](ll: List[List[A]]): List[A] = ll match {
    case Nil => Nil
    case Cons(h, t) => append(h, flatten(t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filterViaPM[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) Cons(h, filter(t)(f))
      else filter(t)(f)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(h, t)) => Cons(h, t)
    case (Cons(h, t), Nil) => Cons(h, t)
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(supH, supT), Cons(subH, subT)) =>
      if (supH == subH) hasSubsequence(supT, subT)
      else hasSubsequence(supT, sub)
    case (_, Nil) => true
    case (_, _) => false
  }
}
