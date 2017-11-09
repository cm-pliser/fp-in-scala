package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def head[A](as: List[A]): A = as match {
    case Cons(a, _) => a
    case Nil => throw new IllegalArgumentException("Passed list is Empty.")
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, tail) => tail
    case Nil => throw new IllegalArgumentException("Passed list is Empty.")
    //head::tail == 元のリストというルールを保てないため例外を投げることを選択した
  }

  def headOption[A](as: List[A]): Option[A] = as match {
    case Cons(a, _) => Some(a)
    case Nil => None
  }

  def tailOption[A](as: List[A]): Option[List[A]] = as match {
    case Cons(_, tail) => Some(tail)
    case Nil => None
  }

  def setHead[A](as: List[A], newHead: A): List[A] =
    Cons(newHead, tail(as))

  def drop[A](as: List[A], n: Int): List[A] =

    n match {
      case _ if n < 0 => throw new IllegalArgumentException("Passed list is Empty.")
      case 0 => as
      case pos => drop(tail(as), pos - 1)
    }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = if (f(head(as))) {
    dropWhile(tail(as), f)
  } else {
    as
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Passed list is Empty.")
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Cons(a, as) => f(a, foldRight(as, z)(f))
    case Nil => z
  }

  def foldRightShortCircuit[A, B](list: List[A], z: => B)(f: (A, =>B) => B): B = list match {
    case Cons(a, as) => f(a, foldRightShortCircuit(as, z)(f))
    case Nil => z
  }

  def foldRightShortCircuit2[A, B](list: List[A], z:() => B)(f: (A, () => B) => B) : B = list match {
    case Cons(a, as) => f(a, () => foldRightShortCircuit2(as, z)(f))
    case Nil => z()
  }

  /*
  NilとConsを特定の値で置き換えるのがfoldRight
  Cons(1, Cons(2, Cons(2, Nil)))
  plus(1, plus(2, plus(3, 0)))
   */
}
