class Point(val x: Int, val y: Int) {
  def +(p: Point): Point = {
    new Point(x + p.x, y + p.y)
  }
  override def toString(): String = "(" + x + ", " + y + ")"

  private val hoge = "hoge"
  def toPrivateString(): String = "private :" + new Point(1, 2).hoge
}

// val p = new Point(1, 2)
// println(p)
// println(p.hoge)
// println(p.toPrivateString)
