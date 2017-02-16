object Lesson10 {

  def count[T](list: List[T])(f: T => Boolean): Int = list match {
    case Nil => 0
    case x::xs => xs.foldLeft(0){(x, y) => if (f(y)) x + 1 else x}
  }
}
