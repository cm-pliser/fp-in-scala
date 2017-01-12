/**
 * Created by suwa-yuki on 2016/11/17.
 */
object Main {

  def main(args: Array[String]): Unit = {

    // 型パラメータ

    class Cell[A](var value: A) {
      def put(newValue: A): Unit = {
        value = newValue
      }

      def get(): A = value
    }

    val cell = new Cell[Int](1)
    cell.put(2)
    println("cell = " + cell.get())

    // Tuple

    val single = new Tuple1[Int](1)
    val pair = new Pair[Int, Int](2, 2)
    val triple = new Triple[Int, Int, Int](3, 3, 3)

    // 演習

    trait Stack[+A] {
      def push[E >: A](e: E): Stack[E]
      def top: A
      def pop: Stack[A]
      def isEmpty: Boolean
    }

    class NonEmptyStack[+A](private val first: A, private val rest: Stack[A]) extends Stack[A] {
      def push[E >: A](e: E): Stack[E] = new NonEmptyStack[E](e, this)
      def top: A = first
      def pop: Stack[A] = rest
      def isEmpty: Boolean = false
    }

    case object EmptyStack extends Stack[Nothing] {
      def push[E >: Nothing](e: E): Stack[E] = new NonEmptyStack[E](e, this)
      def top: Nothing = throw new IllegalArgumentException("empty stack")
      def pop: Nothing = throw new IllegalArgumentException("empty stack")
      def isEmpty: Boolean = true
    }

    object Stack {
      def apply(): Stack[Nothing] = EmptyStack
    }

    val intStack: Stack[Int] = Stack()
    val stringStack: Stack[String] = Stack()
    val nonEmptyStack = new NonEmptyStack[String]("hoge", EmptyStack)
    println("isEmpty : " + nonEmptyStack.isEmpty)
    println("top : " + nonEmptyStack.top)
    println("pop isEmpty : " + nonEmptyStack.pop.isEmpty)

    // 上限限界

    abstract class Show {
      def show: String
    }
    class ShowablePair[A <: Show, B <: Show](val a: A, val b: B) extends Show {
      override def show: String = "(" + a.show + "," + b.show + ")"
    }

    // 下限限界



  }

}