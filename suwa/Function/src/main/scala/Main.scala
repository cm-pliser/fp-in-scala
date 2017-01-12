import scala.io.Source

/**
 * Created by suwa-yuki on 2016/12/01.
 */
object Main {

  def main(args: Array[String]): Unit = {

    // 生の書き方

    val add = new Function2[Int, Int, Int]{
      def apply(x: Int, y: Int): Int = x + y
    }
    println(add(100, 200))

    // 糖衣構文

    val sugarAddaaaaa = (x: Int, y: Int) => x + y

    sugarAddaaaaa(1, 2)
    println(sugarAddaaaaa(300, 400))

    // カリー化

    val adda = (x: Int, y: Int) => x + y
    val addCurried = (x: Int) => (y: Int) => x + y
    println(addCurried(100)(200))

    // 高階関数

    def double(n: Int, f: Int => Int): Int = {
      f(f(n))
    }

    println(double(1, m => m * 2))
    println(double(2, m => m * 3))
    println(double(3, m => m * 4))

    def around(init: () => Unit, body: () => Any, fin: () => Unit): Any = {
      init()
      try {
        body()
      } finally {
        fin()
      }
    }

    around(
      () => println("ファイルを開く"),
      () => println("ファイルに対する処理"),
      () => println("ファイルを閉じる")
    )

    def withFile[A](filename: String)(f: Source => A): A = {
      val s = Source.fromFile(filename)
      try {
        f(s)
      } finally {
        s.close()
      }
    }

    def printFile(filename: String): Unit = {
      withFile(filename) { file =>
        file.getLines.foreach(println)
      }
    }

  }

}
