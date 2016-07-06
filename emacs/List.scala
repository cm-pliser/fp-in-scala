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
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,acc) => acc + 1)

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
    foldRight(la, lb)(Cons.apply)
    // foldRight(la, lb)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  // 3.16
  def addOne(lst: List[Int]): List[Int] =
    foldRight(lst, Nil:List[Int])((elm, acc) => Cons(elm+1, acc))

  // 3.17
  def doubleToStr(lst: List[Double]): List[String] =
    foldRight(lst, Nil:List[String])((elm, acc) => Cons(elm.toString, acc))

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((elm, acc) => Cons(f(elm), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A]){(elm, acc) => if(f(elm)) Cons(elm, acc) else acc}

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))
    // foldRight((map(as)(f)), Nil:List[B])(append)

  // 3.21
  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  // 3.22
  def zip(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, zip(t1,t2))
  }

  // 3.23
  def zipWith[A,B](l1: List[A], l2: List[A])(f:(A,A) => B): List[B] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
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


// 3.8 同じListが返却される
List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

// 3.9
List.length2(List(1,2,3,4,5))

// 3.10
List.foldLeft(List(1,2,3,4,5), 0)(_ + _)

// 3.12
List.reverse(List(1,2,3))

// 3.14
List.append(List(1,2,3), List(4,5,6))

// 3.16
List.addOne(List(1,2,3,4,5))

// 3.17
List.doubleToStr(List(1.0,2.0,3.0,4.0,5.0))

// 3.18
List.map(List(5,6,7,8,9))(_ * 2)

// 3.19
List.filter(List(1,2,3,4,5))(x => x % 2 == 0)

// 3.20
println(List.flatMap(List(1,2,3))(i => List(i,i)))

// 3.21
println(List.flatMapFilter(List(1,2,3,4,5))(x => x % 2 == 0))

// 3.22
println(List.zip(List(1,2,3), List(4,5,6)))

// 3.23
println(List.zipWith(List("h","g","f","g"), List("o","e","u","a"))(_ + _))
