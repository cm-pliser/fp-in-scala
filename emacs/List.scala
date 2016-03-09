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
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // 3.3
  def setHead[A](xs: List[A], other_val: A) = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(other_val, xs)
  }

  // 3.4 問題通りに引数「l」で定義するとパターンマッチどうやる？
  def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case Nil => Nil
    case Cons(l, ls) if n == 1 => ls
    case Cons(l, ls) => drop(ls, n - 1)
  }

  // 3.5 最後までマッチしなかった場合どうする？
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(l, ls) if f(l) => Cons(l, ls)
    case Cons(l, ls) if !f(l) => dropWhile(ls, f)
    case Nil => ls
  }
}

// 3.1
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

// 3.2
val tail = List.tail(List(1,2,3,4,5))

// 3.3
val otherVal = List.setHead(List("Scala", "Ruby", "Elixir"), "PHP")

// 3.4
val dropList = List.drop(List(1,2,3,4,5), 3)

// 3.5
val dropWhileList = List.dropWhile(List(1,2,3,4,5), (x: Int) => x == 3)
