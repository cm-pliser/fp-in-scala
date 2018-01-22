package chapter4

sealed trait Option[+A] {
  def orElse[B >: A](ob: Option[B]): Option[B] =
    this.map(Some[B](_)).getOrElse(ob)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if (f(a)) this else None)
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil)) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def map2[A, B, C](maybeA: Option[A],
                    maybeB: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- maybeA
      b <- maybeB
    } yield f(a, b)

  //maybeA.flatMap{a => maybeB.map{b => f(a, b)}}
}
