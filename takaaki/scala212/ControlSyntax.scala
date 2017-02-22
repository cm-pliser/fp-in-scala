/**
  * https://dwango.github.io/scala_text/control-syntax.html
  */
object ControlSyntax {
  def main(args: Array[String]): Unit = {

    for(a <- 1 to 1000; b <- 1 to 1000; c <- 1 to 1000 if a * a == b * b + c * c) {
      println((a, b, c))
    }

    print('\n')

    for(i <- 1 to 1000) {
      val s = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList match {
        case List(a,b,c,d,_) => List(a,b,c,d,a).mkString
      }
      println(s)
    }
  }
}
