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

  //List(1, 2, 3)
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil)) { (a: A, acc: Option[List[B]]) =>
      map2(f(a): Option[B], acc: Option[List[B]]){(a, as) => a :: as}
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
