package chapter3

sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a)             => a
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)             => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)             => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B, g: (B, B) => B): B = tree match {
    case Leaf(a)             => f(a)
    case Branch(left, right) => g(fold(left)(f, g), fold(right)(f, g))
  }

  def sizeBasedOnFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(_ => 1, (left, right) => left + right + 1)
  def maximumBasedOnFold(tree: Tree[Int]): Int =
    fold[Int, Int](tree)(identity, _ max _)

  def depthBasedOnFold[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(_ => 1, (left, right) => (left max right) + 1)

  def mapBasedOnFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(a => Leaf(f(a)), Branch(_, _))
}
