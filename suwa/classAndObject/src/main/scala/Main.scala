object Main {
  def main(args: Array[String]): Unit = {

    class Point(_x: Int, _y: Int) {
      val x = _x
      val y = _y
      def +(p: Point): Point = {
        new Point(x + p.x, y + p.y)
      }
      override def toString: String = "(" + x + ", " + y + ")"
    }

    val point = new Point(10, 10)
    println(point.toString)

    val added = point.+(new Point(20, 20))
    println(added.toString)

  }
}
