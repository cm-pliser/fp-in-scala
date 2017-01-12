/**
 * Created by suwa-yuki on 2016/12/15.
 */
object Main {

  def main(args: Array[String]): Unit = {

    // Array

    val arr = Array(1, 2, 3, 4, 5)
    arr(0) = 7
    println(arr(0))
    println(arr.length)

    def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
      val tmp = arr(i)
      arr(i) = arr(j)
      arr(j) = tmp
    }

    swapArray(arr)(0, 4)
    println(arr(0))

    // Range

    1 to 5
    val range = (1 to 5).toList
    println(range(0))

    1 until 5
    val untilRange = (1 until 5).toList
    println(untilRange(0))

    // List

    val lst = List(1, 2, 3, 4, 5)

    // ::

    val a1 = 1 :: Nil
    val a2 = 2 :: a1
    val a3 = 3 :: a2
    val a4 = 4 :: a3
    val a5 = 5 :: a3

    1 :: 2 :: 3 :: 4 :: Nil

    // ++

    List(1, 2) ++ List(3, 4)
    List(1) ++ List(3, 4, 5)
    List(3, 4, 5) ++ List(1)

    // mkString

    println(List(1, 2, 3, 4, 5).mkString)
    println(List(1, 2, 3, 4, 5).mkString(","))
    println(List(1, 2, 3, 4, 5).mkString("[", ",", "]"))

    def joinByComma(start: Int, end: Int): String = start to end mkString ","
    println(joinByComma(1, 5))

  }

}