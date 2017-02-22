import scala.io.Source

/**
  * AtCoderのProblem Q1
  */

object Q1 {
  def main(args: Array[String]): Unit = {
    // 1
    val line1 = io.StdIn.readLine().split(' ')

    // 2
    val line2 = io.StdIn.readLine().split(' ')

    // string
    val line3 = io.StdIn.readLine().split(' ')

    val lines = (line1 ++ line2).foldLeft(0)((x, y) => x.toInt + y.toInt)

    println(lines.toString + ' ' + line3.mkString + '\n')
  }
}
