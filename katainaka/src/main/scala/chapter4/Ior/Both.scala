package chapter4.Ior

sealed trait Ior[+E, +A] {
  def flatMap[EE >: E, B](f: A => Ior[EE, B]): Ior[EE, B] = this match {
    case Right(a) => f(a)
    case Both(es1, a) => {
      val fa = f(a)
      fa match {
        case Right(a)     => Both(es1, a)
        case Both(es2, b) => Both(es1 ::: es2, b)
        case Left(es2)    => Left(es1 ::: es2)
      }
    }
    case Left(es) => Left(es)
  }
}

object Ior {
  def map2[E, EE >: E, A, B, C](iorA: Ior[E, A],
                                iorB: Ior[EE, B])(f: (A, B) => C): Ior[EE, C] =
    iorA match {
      case Right(a) =>
        iorB match {
          case Right(b)     => Right(f(a, b))
          case Both(es2, b) => Both(es2, f(a, b))
          case Left(es2)    => Left(es2)
        }
      case Both(es1, a) =>
        iorB match {
          case Right(b)     => Both(es1, f(a, b))
          case Both(es2, b) => Both(es1 ::: es2, f(a, b))
          case Left(es2)    => Left(es1 ::: es2)
        }
      case Left(es1) =>
        iorB match {
          case Right(_)     => Left(es1)
          case Both(es2, _) => Left(es1 ::: es2)
          case Left(es2)    => Left(es1 ::: es2)
        }
    }

  def unit[A](a: A): Ior[Nothing, A] = Right(a)

}

case class Left[+E](errors: List[E]) extends Ior[E, Nothing]

case class Right[+A](value: A) extends Ior[Nothing, A]

case class Both[+E, +A](errors: List[E], value: A) extends Ior[E, A]
