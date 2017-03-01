object Lesson18 {

  trait Additive[A] {
    def plus(a: A, b:A): A
    def zero: A
  }

  def sum[A](list: List[A])(m: Additive[A]) = list.foldLeft(m.zero)((x, y) => m.plus(x, y))

  // AdditiveをString型で定義
  object StringAdditive extends Additive[String] {
    def plus(a: String, b:String): String = a + b // 文字列の連結と定義
    def zero: String = "" // ０は空文字
  }

  // AdditiveをInt型で定義
  object IntAdditive extends Additive[Int] {
    def plus(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 2, 3))(IntAdditive))
    println(sum(List("A", "B", "C"))(StringAdditive))
  }
}
