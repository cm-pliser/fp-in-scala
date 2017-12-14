package chapter3

import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

class ListSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

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

    an[IllegalArgumentException] shouldBe thrownBy {
      drop(values, 6)
    }
  }

  "dropWhile" should "適切に動作する" in {
    val values = List(1, 2, 3, 4, 5)

    dropWhile[Int](values, _ < 3) shouldBe List(3, 4, 5)
  }

  "foldRightShortCurcuit" should "短絡評価する" in {
    val values = List(1, 2, 3, 0, 4, 5, 0)

    var i = 0
    foldRightShortCircuit(values, 1)((a, b) =>
      if (a == 0) 0 else {
        i = i + 1
        a * b
      })

    i shouldBe 3
  }

  "foldRightShortCurcuit2" should "短絡評価する" in {
    val values = List(1, 2, 3, 0, 4, 5, 0)

    var i = 0
    foldRightShortCircuit2(values, () => 1)((a, b) => if (a == 0) 0 else {
      i = i + 1
      a * b()
    })

    i shouldBe 3
  }

  "foldRightBasedOnFoldLeft" should "スタック溢れない" in {
    var acc: List[Int] = Nil
    for (_ <- 1 to 100000) {
      acc = Cons(1, acc)
    }


    foldRightBasedOnFoldLeft3(acc, 0) {
      _ + _
    } shouldBe 100000
  }

  "foldLeftBasedOnFoldRight" should "計算する" in {
    val values = List(1, 2, 3, 0, 4, 5, 0)


    foldLeftBasedOnFoldRight(0, values)(_ + _) shouldBe 15
  }

  implicit def arbList[A](implicit arbA: Arbitrary[A]): Arbitrary[List[A]] = Arbitrary {
    Gen.listOf(arbA.arbitrary).map(List(_: _*))
  }

  def nonEmptyListGen[A](implicit arbA: Arbitrary[A]) = Gen.nonEmptyListOf(arbA.arbitrary).map(List(_: _*))

  "append1ToAllElement" should "全部の要素に1を足す" in {
    val values = List(1, 2, 3)
    append1ToAllElement(values) shouldBe List(2, 3, 4)
  }

  "toStringAllElement" should "文字列にする" in {
    val values = List(1.2, 3.4, 2.0)

    toStringAllElement(values) shouldBe List("1.2", "3.4", "2.0")
  }

  "map" should "空のリストには空" in {
    forAll { (f: String => Int) =>
      map(Nil)(f) shouldBe Nil
    }
  }

  //Functorである
  "map" should "恒等関数では値を変えない" in {
    forAll { (list: List[String]) =>
      map(list)(identity) shouldBe list
    }
  }

  "map" should "可換である" in {
    forAll((list: List[String], f: String => Int, g: Int => Long) =>
      map(map(list)(f))(g) shouldBe map(list)(f andThen g)
    )
  }

  "tailsWithList" should "適切に動作する" in {
    tailsWithList(List(1, 2, 3)) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  "startsWith" should "適切に動作する" in {
    startsWith(List(1, 2, 3), List(1, 2)) shouldBe true
    startsWith(List(1, 2, 3), List(2, 3)) shouldBe false
  }

  "hasSubsequence" should "含む例" in {
    forAll { (prefix: List[String], sub: List[String], suffix: List[String]) => {
      val sup = flatten(List(prefix, sub, suffix))
      hasSubsequence(sup, sub) shouldBe true
    }}
  }

  "hasSubsequence" should "含まない例" in {
    hasSubsequence(List(1, 2, 3), List(1, 3)) shouldBe false
  }
}
