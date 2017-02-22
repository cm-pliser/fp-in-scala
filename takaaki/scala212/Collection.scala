/**
  * https://dwango.github.io/scala_text/collection.html
  */
object Collection {
  def main(args: Array[String]): Unit = {
    val arr = Array(1, 2, 3, 4, 5)
    swapArray(arr)(0, 4)
    print(arr)
    print('\n')

    val str = joinByComma(1, 10)
    print(str)
    print('\n')

    val reverseValue = reverse(List(1, 2, 3, 4, 5))
    print(reverseValue)
    print('\n')

    val sumValue = sum(List(1, 2, 3, 4, 5))
    print(sumValue)
    print('\n')

    val mulValue = mul(List(1, 2, 3, 4, 5))
    print(mulValue)
    print('\n')

    val string = mkString(List(1, 2, 3, 4, 5))(",")
    print(string)
    print('\n')

    val mapValue = map(List(1, 2, 3, 4, 5)){ x => x * 2 }
    print(mapValue)
    print('\n')

    val filterValue = filter(List(1, 2, 3, 4, 5)){ x => x % 2 == 0 }
    print(filterValue)
    print('\n')

    val countValue = count(List(1, 2, 3, 4, 5)){ x => x % 2 == 0 }
    print(countValue)
    print('\n')
  }

  def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }

  def joinByComma(start: Int, end: Int): String = {
    (start to end).mkString(",")
  }

  def reverse[T](list: List[T]): List[T] = list.foldLeft(Nil: List[T])((a, b) => b :: a)

  def sum(list: List[Int]): Int = list.foldRight(0) { (x, y) => x + y }

  def mul(list: List[Int]): Int = list.foldRight(1) { (x, y) => x * y }

  def mkString[T](list: List[T])(sep: String): String = list match {
    case Nil => ""
    case x::xs => xs.foldLeft(x.toString) { (x, y) => x + sep + y }
  }

  def map[T, U](list: List[T])(f: T => U): List[U] = {
    list.foldLeft(Nil:List[U]) { (x, y) => f(y) :: x }.reverse
  }

  def filter[T](list: List[T])(f: T => Boolean): List[T] = {
    list.foldLeft(Nil:List[T]) { (x, y) => if (f(y)) y::x else x }.reverse
  }

  def count[T](list: List[T])(f: T => Boolean): Int  = {
    list.foldLeft(0) { (x, y) => if (f(y)) x + 1 else x }
  }
}
