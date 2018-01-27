package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = fold(tree)(a => 1)((b1, b2) => 1 + b1 + b2)

  def maximum(tree: Tree[Int]): Int = fold(tree)(a => a)((b1, b2) => math.max(b1, b2))

  def depth[A](tree: Tree[A]): Int = fold(tree)(a => 1)((b1, b2) => 1 + math.max(b1, b2))

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)).asInstanceOf[Tree[B]])((b1, b2) => Branch(b1, b2))

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(lt, rt) => b(fold(lt)(l)(b), fold(rt)(l)(b))
  }
}