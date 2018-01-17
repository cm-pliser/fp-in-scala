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

        println("Exercise 3-16: add 1")
        println(List.add1(List(1, 5, 10, 15)))

        println("Exercise 3-17: List[Double] to List[String]")
        println(List.listToString(List(1.0, 2.0, 3.3, 4.5)))

        println("Exercise 3-18: map")
        val hoge = List.map(List(1.1, 2.2, 3.3, 4.4, 5.5))(x => x.toString() + " HOGE")
        println(hoge)


        println("Exercise 3-19: Filter")
        val fuga = List.filter(List(1, 11, 22, 41, 54, 21))(x => x % 2 == 0)
        println(fuga)
    }
}