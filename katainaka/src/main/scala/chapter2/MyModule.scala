package chapter2

import scala.annotation.tailrec

object MyModule {
  def fib(n: Int): Int = {
    @tailrec
    def go(prev: Int, current: Int, nth: Int): Int =
      nth match {
        case 0 => prev
        case 1 => current
        case _ => go(current, prev + current, nth - 1)
      }

    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def go(indexOfFirst: Int): Boolean =
      indexOfFirst >= as.length - 1 ||
        ordered(as(indexOfFirst), as(indexOfFirst + 1)) && go(indexOfFirst + 1)

    go(0)
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def go(indexOfFirst: Int): Boolean = {
      val result = for {
        second <- as.lift(indexOfFirst + 1)
        first <- as.lift(indexOfFirst)
      } yield ordered(first, second)

      result match {
        case None => true //配列の最後まで見た
        case Some(false) => false //順番通り並んでいなかった
        case Some(true) => go(indexOfFirst + 1) //ここはオッケーなので次へ
      }
    }

    go(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
