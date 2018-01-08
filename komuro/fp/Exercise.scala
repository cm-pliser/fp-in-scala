object Exercise {

  // 素直に定義を書いただけ
  def fib(n: Int): Int = {

    def loop(count: Int, acc: Int): Int = {
      if (count <= 0) acc
      else if (count == 1) 0
      else if (count == 2) 1
      else loop(count - 1, acc) + loop(count -2, acc)
    }

    loop(n, 0)
  }

  // 配列内の要素を検索する多相関数
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {

    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // 頭わるすぎでは・・・
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Either[Throwable, Boolean] = {
    def loop(n:Int, res: Boolean): Either[Throwable, Boolean] =
      if (n > as.length - 1) Left(new Exception("can not Compare"))
      else if (n == as.length - 1) Right(res)
      else loop(n + 1, (ordered(as(n), as(n + 1))))

    loop(0, true)
  }

  // 部分関数
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // カリー化
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) =
    (a: A) => (b: B) => f(a, b) 

  def curryA[A, B, C](f: (A, B) => C) : A => (B => C) =
    (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)


  def main(args: Array[String]): Unit = {
    println(fib(5))
    println(fib(6))
    println(fib(7))

    println(isSorted(Array(2, 3, 5, 6, 0), (a:Int, b:Int) => a < b))
    println(isSorted(Array(1, 9, 11, 244, 256), (a:Int, b:Int) => a < b))
    println(isSorted(Array(256), (a:Int, b:Int) => a < b))
    
    val curriedFunction = curry((x: Int, y:Int) => x + y)
    println("Curry後 : " + curriedFunction(2)(3))

    var uncurryFunction = uncurry(curriedFunction)
    println("Uncurry後 : " + uncurryFunction(2, 3))
  }
}
