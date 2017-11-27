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

  "drop" should "適切に動作する" in {
    val values = List(1, 2, 3, 4, 5)

    drop(values, 2) shouldBe List(3, 4, 5)
  }

  "drop" should "大きい値はさようなら" in {
    val values = List(1, 2, 3, 4, 5)

    an[IllegalArgumentException] shouldBe thrownBy{drop(values, 6)}
  }

  "dropWhile" should "適切に動作する" in {
    val values = List(1, 2, 3, 4, 5)

    dropWhile[Int](values, _ < 3) shouldBe List(3, 4, 5)
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

   "foldRightBasedOnFoldLeft" should "計算する" in {
    val values = List(1, 2, 3, 0, 4 , 5, 0)


    foldRightBasedOnFoldLeft(values, 0){_ + _} shouldBe 15
  }

  "foldLeftBasedOnFoldRight" should "計算する" in {
    val values = List(1, 2, 3, 0, 4 , 5, 0)


    foldLeftBasedOnFoldRight(0, values){_ + _} shouldBe 15
  }


}
