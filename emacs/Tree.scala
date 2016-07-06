sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def foldMaximum(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)
}



println(Tree.map(
  Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Leaf(4))))
  (x => x + 1)
)

println(Tree.foldSize(
  Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Leaf(4)))))

println(Tree.maximum(
  Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Leaf(4)))))
