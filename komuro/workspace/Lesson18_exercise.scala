object Lesson18_exercise {

  trait Additive[A] {
    def plus(a: A, b:A): A
    def zero: A
  }

  case class Point(x: Int, y:Int)

  implicit object divisionAdditive extends Additive[Point] {
    def plus(p1: Point, p2: Point): Point = Point(p1.x + p2.x, p1.y + p2.y)
    def zero: Point = Point(0, 0)
  }

  // m: Additive[A]に implicit を付与することで上記のimplicit parameterを推論してくれる?
  def sum[A](list: List[A])(implicit m: Additive[A]) = list.foldLeft(m.zero)((x, y) => m.plus(x, y))

  def main(args: Array[String]): Unit = {
    println(sum(List(Point(1, 2), Point(3, 4))))
    println("(t1 + (t2 + t3)) = " + sum(
      List(Point(1, 2), sum(
        List(Point(3, 4), Point(5, 6))))))

    println("((t1 + t2) + t3) = " + sum(
      List(
        sum(List(Point(1, 2), Point(3,4))),
        Point(5, 6))))
  }

  // List[Int]とList[Double]がsumを取れるように定義してる型クラスは何
  // - List         http://www.scala-lang.org/api/current/scala/collection/immutable/List.html
  // - Numeric[T]   http://www.scala-lang.org/api/current/scala/collection/immutable/List.html#sum:A
  // Numeric        http://www.scala-lang.org/api/current/scala/math/Numeric.html
  //                https://github.com/scala/scala/blob/v2.12.1/src/library/scala/math/Numeric.scala#L1
  // DoubleIsAsIntegral
  // IntIsIntegral
}
