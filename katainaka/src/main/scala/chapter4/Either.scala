package chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))
  }
  def flatMap[B, E2 >: E](f: A => Either[E2, B]): Either[E2, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }
  def orElse[B >: A, E2 >: E](other: Either[E2, B]) = this match {
    case Left(_)  => other
    case Right(a) => Ior.Right(a)
  }
  def map2[B, E2 >: E, C](other: Either[E2, B])(f: (A, B) => C): Either[E2, C] =
    for {
      a <- this
      b <- other
    } yield f(a, b)
}

object Either {
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil)) { (a, acc) =>
      f(a).map2(acc)(_ :: _)
    }

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    traverse(a)(identity)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
