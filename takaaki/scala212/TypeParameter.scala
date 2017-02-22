import scala.io.Source

/**
  * https://dwango.github.io/scala_text/type-parameter.html
  */
object TypeParameter {
  def main(args: Array[String]): Unit = {
    val intStack: Stack[Int] = Stack()
    val stringStack: Stack[String] = Stack()

    print(intStack)
    print(stringStack)
  }

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
}
