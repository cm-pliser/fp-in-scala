import Stream._

trait Stream[+A] {
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  // def exists(p: A => Boolean): Boolean = this match {
  //   case Cons(h, t) => p(h()) || t().exists(p)
  //   case _ => false
  // }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileFoldr(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else empty)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def mapUnfold[B](f:A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(),n-1)))
      case _ => None
    }

  def takeWhileUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithUnfold[B,C](z2: Stream[B])(f:(A,B) => C): Stream[C] =
    unfold((this, z2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // ここ全くわからん
  def zipAllUnfold[B](z2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAllUnfold[B,C](z2)((_,_))

  def zipWithAll[B,C](z2: Stream[B])(f:(Option[A],Option[B]) => C): Stream[C] =
    Stream.unfold((this,z2)) {
      case (Empty,Empty) => None
      case (Cons(h,t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h,t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case(Cons(h1,t1), Cons(h2,t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def tailsUnfold: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail:Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def fibsUnfold =
    unfold((0,1)) { case (f0, f1) => Some((f0, (f1, f0+f1))) }

  def fromUnfold(n: Int) =
    unfold(n) { n => Some(n, n+1) }

  def constantUnfold[A](a: A) =
    unfold(a)(_ => Some((a,a)))

  val onesUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))

}

println(Stream(1,2,3).take(3).toList)

println(Stream.onesUnfold.take(5).toList)

println(Stream.unfold((0,1)) { case (f0,f1) => Some((f0, (f1, f0+f1))) }.take(5).toList)

println(Stream(1,2,3).mapUnfold(_ * 2).toList)

println(Stream(1,2,3,4,5).takeUnfold(3).toList)

println(Stream(1,2,3,4,5).takeWhileUnfold(_ < 3).toList)

println(Stream(1,2,3,4,5).zipWithUnfold(Stream(1,2,3,4,5))(_ + _).toList)
