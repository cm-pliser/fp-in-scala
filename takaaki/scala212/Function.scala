import scala.io.Source

/**
  * https://dwango.github.io/scala_text/function.html
  */
object Function {
  def main(args: Array[String]): Unit = {
    printFile("test.txt")
  }

  def printFile(filename: String): Unit = {
    withFile(filename) { file =>
      file.getLines.foreach(println)
    }
  }

  def withFile[A](filename: String)(f: Source => A): A = {
    val s = Source.fromFile(filename)
    try {
      f(s)
    } finally {
      s.close()
    }
  }
}

