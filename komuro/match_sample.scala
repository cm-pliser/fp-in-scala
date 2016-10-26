def random5():Unit = {
  for (i <- 1 to 10) {
    val charList = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList match {
      case List(a, b, c, d, _) => {
        List(a, b, c, d, a)
      }
    }
    println(charList)
  }
}
random5()
