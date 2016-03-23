//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil // 例外にするか、引数に受け付けないようにする
    // case Cons(x, Nil) => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, Nil) => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, _) => if (f(x)) dropWhile(tail(l), f) else l
  }

  // List 3-2
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => append(Cons(x, Nil), init(xs))
  }
}
