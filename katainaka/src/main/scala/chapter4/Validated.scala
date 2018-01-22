package chapter4

sealed trait Validated[+E, +A] {

  //Aplicative
  def orElse[EE >: E, B >: A](that: Validated[EE, B]): Validated[EE, B] =
    (this, that) match {
      case (Valid(a), _)                => Valid(a)
      case (_, Valid(b))                => Valid(b)
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
    }

  def map[B](f: A => B): Validated[E, B] =
    Validated.map2(this, Valid(()))((a, _) => f(a))

  //Monadic
  def flatMap[B, EE >: E](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(a)   => f(a)
      case Invalid(e) => Invalid(e)
    }
}

object Validated {
  //Aplicative
  def unit[A](a: A): Validated[Nothing, A] = Valid(a)

  def map2[E1, E2 >: E1, A, B, C](a: Validated[E1, A], b: Validated[E2, B])(
    f: (A, B) => C
  ): Validated[E2, C] =
    (a, b) match {
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ::: es2)
      case (Invalid(es1), _)            => Invalid(es1)
      case (_, Invalid(es2))            => Invalid(es2)
      case (Valid(a), Valid(b))         => Valid(f(a, b))
    }

  def traverse[E, A, B](as: List[A])(f: A => Validated[E, B]) =
    as.foldRight[Validated[E, List[B]]](Valid(Nil))(
      (a, acc) => Validated.map2(f(a), acc) { _ :: _ }
    )

  def sequence[E, A](as: List[Validated[E, A]]): Validated[E, List[A]] =
    traverse(as)(identity)

}

case class Invalid[+E](errors: List[E]) extends Validated[E, Nothing]

case class Valid[+A](value: A) extends Validated[Nothing, A]
