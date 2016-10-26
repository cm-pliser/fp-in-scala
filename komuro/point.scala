class Point(val x: Int, val y: Int) {
  def +(p: Point): Point = {
    new Point(x + p.x, y + p.y)
  }
  override def toString(): String = "(" + x + ", " + y + ")"

  // private[this] val hoge = "hoge"
  // def toPrivateString(): String = "private :" + hoge
}

val p = new Point(1, 2)
println(p)
// println(p.hoge)
// println(p.toPrivateString)
