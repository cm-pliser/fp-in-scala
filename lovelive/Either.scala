object either {

  sealed trait \/[+A, +B] {
    def map[C](f: B => C): \/[A, C] = this match {
      case l @ -\/(_) => l
      case \/-(b) => \/-(f(b))
    }

    def flatMap[AX >: A, C](f: B => \/[AX, C]): \/[AX, C] = this match {
      case l @ -\/(_) => l
      case \/-(a) => f(a)
    }

    def orElse[AX >: A, BX >: B](b: => \/[AX, BX]): \/[AX, BX] = this match {
      case -\/(_) => b
      case r @ \/-(_) => r
    }

    def map2[AX>: A, C, D](ec: \/[AX, C])(f: (B, C) => D): \/[AX, D] =
      flatMap { b => ec.map { c => f(b, c) } }
  }

  final case class -\/[A](get: A) extends (A \/ Nothing)
  final case class \/-[B](get: B) extends (Nothing \/ B)

}
