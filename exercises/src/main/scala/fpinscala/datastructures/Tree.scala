package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => fold(l, fold(r, z)(f))(f)
    }

  def max(t: Tree[Int]): Int =
    fold(t, Int.MinValue)((a, b) => Math.max(a, b))

  def sum(t: Tree[Int]): Int =
    fold(t, 0)(_ + _)

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => Math.max(1 + depth(l), 1 + depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}