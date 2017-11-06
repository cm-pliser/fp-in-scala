package chapter3

import org.scalatest.{ FlatSpec, Matchers }

class ListSpec extends FlatSpec with Matchers {

  import List._

  "パターンマッチ" should "試してみた" in {
    val result = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    result shouldBe 3
  }

  "foldRightShortCurcuit" should "短絡評価する" in {
    val values = List(1, 2, 3, 0, 4 , 5, 0)

    var i = 0
    foldRightShortCircuit(values, 1)((a, b) => if(a == 0) 0 else {
      i = i + 1
      a * b
    })

    i shouldBe 3
  }

  "foldRightShortCurcuit2" should "短絡評価する" in {
    val values = List(1, 2, 3, 0, 4 , 5, 0)

    var i = 0
    foldRightShortCircuit2(values,() => 1)((a, b) => if(a == 0) 0 else {
      i = i + 1
      a * b()
    })

    i shouldBe 3
  }

}