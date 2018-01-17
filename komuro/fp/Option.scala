sealed trait Option[+A] {

    /** Exercise 4-4 */
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    def flatMap[B](f:A => Option[B]): Option[B] 

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f:A => Boolean): Option[A]
}

object Option {
        
}