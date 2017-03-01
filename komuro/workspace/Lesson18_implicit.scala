object Lesson18_implicit {

  // plusとzero定義
  trait Additive[A] {
    def plus(a: A, b:A): A
    def zero: A
  }

  // AdditiveをString型で定義
  implicit object StringAdditive extends Additive[String] {
    def plus(a: String, b:String): String = a + b // 文字列の連結と定義
    def zero: String = "" // ０は空文字
  }

  // AdditiveをInt型で定義
  implicit object IntAdditive extends Additive[Int] {
    def plus(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

  // m: Additive[A]に implicit を付与することで上記のimplicit parameterを推論してくれる?
  def sum[A](list: List[A])(implicit m: Additive[A]) = list.foldLeft(m.zero)((x, y) => m.plus(x, y))

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 2, 3)))
    println(sum(List("A", "B", "C")))


// Lesson18_implicit.scala:28: error: could not find implicit value for parameter m: Main.Additive[Double]
// println(sum(List(1.0, 2.0, 3.0)))
//                ^
// one error found
// と言われてOut ↓
    // println(sum(List(1.0, 2.0, 3.0)))
  }
}
