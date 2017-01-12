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


    def reverse[T](list: List[T]): List[T] = list.foldLeft(Nil: List[T])((a, b) => b :: a)
    println("Reversed : " + reverse(Nil))


    // map

    val mapped = List(1, 2, 3, 4, 5).map(x => x * 2)
    println("Mapped : " + mapped)

    // 練習問題

    // T と U は型パラメータ
    // T は 引数の List で与えられた要素の型になる
    // U は map の結果、戻り値となる List の要素の型になる
    def map[T, U](list: List[T])(f: T => U): List[U] = {
      list.foldLeft(Nil:List[U]){
        // x, y を引数に取る
        // f を y を引数として実行し、:: (コンス) で x を付ける
        // x は適用済みの結果
        (x, y) => f(y) :: x
        // コンスで連結する関係で、逆順になってるので戻す
      }.reverse
//      list.foldLeft(Nil:List[U]){(x, y) => f(y) :: x}.reverse
    }

    val mapped2 = map(List(1, 2, 3, 4, 5))(x => x * 10)
    println("Mapped : " + mapped2)


    // filter

    val filtered = List(1, 2, 3, 4, 5).filter(x => x % 2 == 1)
    println("Filtered : " + filtered)

    // 練習問題

    def filter[T](list: List[T])(f: T => Boolean): List[T] = {
      list.foldLeft(Nil:List[T]){
        (x, y) => {
          // y を引数にした f の結果が true であれば足す
          if (f(y)) {
            y :: x
          } else {
            x
          }
        }
      }.reverse
      list.foldLeft(Nil:List[T]){(x, y) => if(f(y)) y :: x else x}.reverse
    }

    val filtered2 = filter(List(1, 2, 3, 4, 5))(x => x % 2 == 1)
    println("Filtered : " + filtered2)

  }

}