object MyObject {
   def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
     def loop(n :Int): Boolean =
       if (n + 1 >= as.length) true
       else if (ordered(as(n), as(n + 1)) loop(n + 1)
       else false

     loop(0)
   }

   def main(args: Array[String]): Unit = {
     if(isSorted[Int]([1,2,3,4,5], (x: Int, y: Int) => x <= y)) println("sorted!!!")
   }
}
