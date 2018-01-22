package chapter2

import scala.annotation.tailrec

object MyModule {
  def fib(n: Int): Int = {
    @tailrec
    def go(current: Int, next: Int, nth: Int): Int =
      nth match {
        case 0             => current
        case 1             => next
        case _ if nth >= 2 => go(next, current + next, nth - 1)
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
        case None        => true //配列の最後まで見た
        case Some(false) => false //順番通り並んでいなかった
        case Some(true)  => go(indexOfFirst + 1) //ここはオッケーなので次へ
      }
    }

    go(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    f(_: A)(_: B)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
