/**
 * Created by suwa-yuki on 2017/02/02.
 */
object Main {

  def main(args: Array[String]) {

    val a = scala.io.StdIn.readInt()
    val bc = scala.io.StdIn.readLine().split(" ").map(_.toInt).sum
    val s = scala.io.StdIn.readLine()

    println((a + bc) + s)

//    val p = scala.io.StdIn.readLine split " "
//    val a = p(0).toInt
//    val b = p(2).toInt
//    println(if (p(1) == "+") { a + b } else { a - b })

  }

}
