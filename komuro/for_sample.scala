def pythagoras(): Unit = {
  for (a <- 1 to 1000; b <- 1 to 1000; c <- 1 to 1000 if Math.pow(a, 2) == Math.pow(b, 2) + Math.pow(c, 2)) {
    println("a=" + a + ", b=" + b + ", c=" + c)
  }
}
pythagoras()
