object Ex3 {

    def main(args: Array[String]): Unit = {
        println(List.x)
        println(List.drop(List(1, 2, 3, 4, 5), 2))
        println(List.drop(List(1, 2, 3, 4, 5), 5))
        // println(List.drop(List(1, 2, 3, 4, 5), 6))

        println(List.dropWhile(List(1, 2, 3, 4, 5), (x:Int) => x < 3))
        println(List.init(List(1, 2, 3, 4)))
        println(List.ex3_8())
        println(List.length(List(10, 20, 30, 40, 50, 60)))
        println(List.foldLeft(List(1, 2, 3), Nil:List[Int])((x, y) => Cons(y ,x)))

        println(List.sum3(List(1, 3, 5, 7, 9)))
        println(List.product3(List(2, 4, 6, 8, 10)))
        println(List.length2(List(11, 22, 33, 44, 55, 66)))
        println(List.reverse(List(1, 3, 5, 7, 9, 11)))
        
        println("Exercise 3-13: foldRight via foldLeft")
        println(List.foldRightByfoldLeft(List(1, 5, 9, 13, 17, 21), 2)((x, y) => y  * 2))

        println("Exercise 3-14: foldLeft")
        println(List.apeendByfoldLeft(List(1, 3, 5, 7), List(2, 4, 6, 8)))
        println(List.appendViaFoldRight(List(1, 3, 5, 7), List(2, 4, 6, 8)))
    }
}