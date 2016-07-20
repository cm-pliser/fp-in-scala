object stream {
  sealed trait Stream[+A] {
    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = {
      def loop(as: Stream[A], count: Int): Stream[A] = (as, count) match {
        case (_, c) if c <= 0 => empty
        case (Empty, _) => empty
        case (Cons(h, t), c) => cons(h(), loop(t(), c - 1))
      }

      loop(this, n)
    }

    def drop(n: Int): Stream[A] = {
      def loop(as: Stream[A], count: Int): Stream[A] = (as, count) match {
        case (_, c) if c <= 0 => as
        case (Empty, _) => as
        case (Cons(h, t), c) => loop(t(), c - 1)
      }

      loop(this, n)
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      def loop(as: Stream[A]): Stream[A] = as match {
        case Empty => empty
        case Cons(h, t) if p(h()) => cons(h(), loop(t()))
        case _ => empty
      }

      loop(this)
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists2(p: A => Boolean): Boolean =
      foldRight(false) { (a, acc) => println("aa!"); p(a) || acc } // foldRightだからこその芸当

    def forAll(f: A => Boolean): Boolean =
      foldRight(true)((a, acc) => f(a) && acc)

    def takeWhile_1(f: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, acc) =>
        if (f(a)) cons(a, acc)
        else empty
      }

    def headOption_1: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B]) { (a, acc) => cons(f(a), acc) }

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, acc) =>
        if (p(a)) cons(a, acc)
        else acc
      }

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s) { (b, acc) => cons(b, acc) }

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B]) { (b, acc) => f(b).append(acc) }

  }

  case object Empty extends Stream[Nothing]
  final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => hd, () => tl)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    def ones: Stream[Int] = cons(1, ones)

    def constant[A](a: A): Stream[A] = cons(a, constant(a)) // how about laziness or self-reference

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  }
}


