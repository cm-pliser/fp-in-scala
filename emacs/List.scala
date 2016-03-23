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
    case Cons(_, xs) => xs
    // case Cons(x, xs) => xs
  }

  // 3.3
  def setHead[A](xs: List[A], otherVal: A): List[A] = xs match {
    case Nil => Nil
    case Cons(_, xs) => Cons(otherVal, xs)
    // case Cons(x, xs) => Cons(other_val, xs)
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

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ints: List[Int]): Int = {
    foldRight(ints, 0)(_ + _)
  }

  def product2(ds: List[Double]): Double = {
    foldRight(ds, 1.0)(_ * _)
  }

  // 3.7
  // できない、一度リストをすべて操作してから関数実行するため

  // 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_,acc) => acc + 1)
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  // 3.11
  def sum3(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def product3(ints: List[Double]): Double = {
    foldLeft(ints, 1.0)(_ * _)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc,_) => acc + 1)
  }

  // 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((acc, elm) => Cons(elm, acc))
  }

  // 3.14
  def append[A](la: List[A], lb: List[A]): List[A] =
    foldRight(la, lb)(Cons(_,_))

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


// 3.8 同じListが返却される
println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

// 3.9
println(List.length2(List(1,2,3,4,5)))

// 3.10
println(List.foldLeft(List(1,2,3,4,5), 0)(_ + _))

// 3.12
println(List.reverse(List(1,2,3)))

// 3.14
println(List.append(List(1,2,3), List(4,5,6)))
