
object e2_1 {
  
  def fib(n: Long): Long = {
    @annotation.tailrec
    def go(n: Long, prev: Long, current: Long): Long =
      if (n == 0) prev
      else go(n - 1, current, prev + current) 
    go(n, 0, 1)
  }

}
