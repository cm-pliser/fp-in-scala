package chapter3

import chapter2.MyModule

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] = {
    as.reverse.foldLeft[List[A]](Nil) { (b, a) => Cons(a, b) }
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

  //実装してないがtakeがあるはず
  // length(take(list, n)) should be n コレが満たせない場合短い場合例外投げる
  // take(n, list) ++ drop(n, list) should be list
  //drop(Cons(3, Cons(2, Cons(1, Nil))), 3) は
  //drop(Cons(2, Cons(1, Nil)), 2)
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
  n match {
    case _ if n < 0 => throw new IllegalArgumentException("Negative number is passed")
    case 0 => as
    case _ => as match {
      case Cons(_, t) => drop(t, n - 1)
      case Nil => throw new IllegalArgumentException("Passed list is smaller than n")
    }
  }

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => as //asがNilの場合とfでfalseが返る場合
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Passed list is Empty.")
    case Cons(_, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Cons(a, as) => f(a, foldRight(as, z)(f))
    case Nil => z
  }

  def foldRightShortCircuit[A, B](list: List[A], z: => B)(f: (A, => B) => B): B = list match {
    case Cons(a, as) => f(a, foldRightShortCircuit(as, z)(f))
    case Nil => z
  }

  def foldRightShortCircuit2[A, B](list: List[A], z: () => B)(f: (A, () => B) => B): B = list match {
    case Cons(a, as) => f(a, () => foldRightShortCircuit2(as, z)(f))
    case Nil => z()
  }

  @tailrec
  def foldLeft[A, B](z: B, list: List[A])(f: (B, A) => B): B = list match {
    case Cons(a, as) => foldLeft[A, B](f(z, a), as)(f)
    case Nil => z
  }

  def sumByFoldLeft(list: List[Int]): Int = foldLeft(0, list)(_ + _)

  def productByFoldLeft(list: List[Int]): Int = foldLeft(1, list)(_ * _)

  def lengthByFoldLeft(list: List[Int]): Int = foldLeft(0, list)((b, _) => b + 1)

  def reverse[A](list: List[A]): List[A] = foldLeft[A, List[A]](Nil, list)((b, a) => Cons(a, b))


  def foldRightBasedOnFoldLeft2[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B => B](identity, list)((next, a) => (f(a, _: B)) andThen next)(z)

  def foldRightBasedOnFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B => B](identity, list)((next, a) => b => next(f(a, b)))(z)

  def foldRightBasedOnFoldLeft3[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    val reversed = reverse(list) // reverseはfoldLeftで実装されている
    foldLeft(z, reversed)((b, a) => f(a, b))
  }


  def foldLeftBasedOnFoldRight[A, B](z: B, list: List[A])(f: (B, A) => B): B =
    foldRight[A, B => B](list, identity)((a, acc) => (f(_: B, a)) andThen acc)(z)

  // z * (1 * 2 , 3 x) x => (((x * 1) * 2) * 3)
  def foldLeftBasedOnFoldRight2[A, B](z: B, list: List[A])(f: (B, A) => B): B =
    foldRight[A, B => B](list, identity)((a, next) => b => next(f(b, a)))(z)

  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(Cons(_, _))

  /*
  NilとConsを特定の値で置き換えるのがfoldRight
  Cons(1, Cons(2, Cons(2, Nil)))
  plus(1, plus(2, plus(3, 0)))
   */

  def flatten[A](a: List[List[A]]): List[A] = foldLeft(Nil: List[A], a)(append)

  //Scalaは正格評価であるため、この書き方ではスタック溢れの問題がある
  def append1ToAllElement(as: List[Int]): List[Int] =
    as match {
      case Cons(head, tail) => Cons(head + 1, append1ToAllElement(tail))
      case Nil => Nil
    }

  def toStringAllElement(as: List[Double]): List[String] =
    as match {
      case Cons(head, tail) => Cons(head.toString, toStringAllElement(tail))
      case Nil => Nil
    }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
      case Nil => Nil
    }


  def filter[A](as: List[A])(cond: A => Boolean): List[A] =
    as match {
      case Cons(head, tail) if cond(head) => Cons(head, filter(tail)(cond))
      case Cons(head, tail) => filter(tail)(cond)
      case Nil => Nil
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]):List[B] =
    flatten(map(as)(f))

  def filterBasedOnFlatMap[A](as: List[A])(cond: A => Boolean): List[A] =
    flatMap(as){
      case a if cond(a) => List(a)
      case _ => Nil
    }

  def zipAndAdd(as:List[Int], bs:List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) | (_ , Nil) => Nil
      case (Cons(aHead, aTail), Cons(bHead, bTail)) =>
        Cons(aHead + bHead, zipAndAdd(aTail, bTail))
    }

  def zipWith[A, B, C](as: List[A], bs:List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) | (_ , Nil) => Nil
      case (Cons(aHead, aTail), Cons(bHead, bTail)) =>
        Cons(f(aHead ,bHead), zipWith(aTail, bTail)(f))
    }

  def scanRight[A, B](list: List[A], z: B)(f: (A, B) => B): List[B] = {
    foldRight(list, List(z))((a, listB) => Cons(f(a, head(listB)) , listB))
  }

  def tailsWithList[A](list: List[A]): List[List[A]] = scanRight[A, List[A]](list, Nil)(Cons(_, _))

  def exists[A](list: List[A])(cond: A => Boolean):Boolean =
    list match {
      case Cons(head, tail) => cond(head) || exists(tail)(cond)
      case Nil => false
    }

  def startsWith[A](list: List[A], pattern: List[A]): Boolean = {
    (list, pattern) match {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => aHead == bHead && startsWith(aTail, bTail)
      case (_, Nil) => true
      case (Nil, _) => false
    }
  }

  //hasSequence(List(1, 2, 3), List(2, 3))

  //1. exists(List(List(1, 2, 3), List(2, 3), List(3), List()))(startsWith(_, sub)

  //2. それぞれがsubで始まるかを検査しsubで始まっていたらオッケー

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    exists(tailsWithList(sup))(startsWith(_, sub))


}
