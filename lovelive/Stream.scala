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

    def uMap[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }

    def uTake(n: Int): Stream[A] =
      unfold((n, this)) {
        case (c, Cons(h, t)) if c > 0 => Some(h(), (c - 1, t()))
        case _ => None
      }

    def uTakeWhile(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }

    def uZipWith[B, C](sb: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, sb)) {
        case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
        case _ => None
      }

    def uZipAll[B](sb : Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, sb)) {
        case (Empty, Empty) => None
        case (Cons(ha, ta), Empty) =>
          val v = (Some(ha()), Option.empty[B])
          val rest = (ta(), empty[B])
          Some(v, rest)
        case (Empty, Cons(hb, tb)) =>
          val v = (Option.empty[A], Some(hb()))
          val rest = (empty[A], tb())
          Some(v, rest)
        case (Cons(ha, ta), Cons(hb, tb)) =>
          val v = (Some(ha()), Some(hb()))
          val rest = (ta(), tb())
          Some(v, rest)
      }

    def startsWith[A](s: Stream[A]): Boolean =
      uZipAll(s)
        .uTakeWhile {
          case (_, Some(_)) => true
          case _ => false
        }
        .forAll {
          case (ax, ay) => ax == ay
        }

    def tails: Stream[Stream[A]] =
      unfold(this) {
        case s @ Cons(_, t) => Some(s, t())
        case _ => None
      }.append(Stream(empty))

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    def scanRightByUnfold[B](z: => B)(f: (A, => B) => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => ??? /* how can we apply `f` here? */
        case _ => ???
      }

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = { // this signature is more like `foldRight`
      val (stream, _) = foldRight( (Stream(z), z) ) { 
        case (a, (sAcc, aAcc)) =>
          lazy val v = f(a, aAcc)
          (cons(v, sAcc), v)
      }
      stream
    }

    def sTails: Stream[Stream[A]] =
      scanRight(empty[A])(cons(_, _))
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

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None => empty
      }

    val uFibs: Stream[BigInt] =
      unfold( (BigInt(0), BigInt(1)) ) {
        case (prev, cur) =>
          val next = prev + cur
          Some(prev, (cur, next))
      }

    def uFrom(n: Int): Stream[Int] =
      unfold(n) { x => Some(x, x + 1) }

    def uConstant[A](a: A): Stream[A] =
      unfold(a)(Function.const(Some(a, a)))

    val uOnes: Stream[Int] =
      unfold(0)(Function.const(Some(1, 1))) // uConstant(1)
  }
}


