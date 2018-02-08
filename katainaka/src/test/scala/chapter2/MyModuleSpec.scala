package chapter2

import org.scalatest.{FlatSpec, Matchers}

class MyModuleSpec extends FlatSpec with Matchers {

  import chapter2.MyModule._

  "fib()" should "0, 1, 1, 2, 3, 5から始まる" in {
    List(0, 1, 1, 2, 3, 5).zipWithIndex.map {
      case (nthFib, n) =>
        fib(n) shouldBe nthFib
    }
  }

  "isSorted" should "[1,2,3]にはtrue" in {
    isSorted(Array(1, 2, 3), (x: Int, y: Int) => x <= y) shouldBe true
  }

  "isSorted" should "[3, 2, 1]にはfalse" in {
    isSorted(Array(3, 2, 1), (x: Int, y: Int) => x <= y) shouldBe false
  }

  "isSorted2" should "[1, 2, 3]にはtrue" in {
    isSorted2(Array(1, 2, 3), (x: Int, y: Int) => x <= y) shouldBe true
  }

  "isSorted2" should "[3, 2, 1]にはfalse" in {
    isSorted2(Array(3, 2, 1), (x: Int, y: Int) => x <= y) shouldBe false
  }

  "curry" should "元の関数と戻り値は同じ" in {

    def f(value: Double, multiplier: Int): String =
      (value * multiplier).toString

    val x = 10.4
    val y = 3
    f(x, y) shouldEqual curry(f)(x)(y)
  }

  "uncurry" should "元の関数と戻り値は同じ" in {

    def f(value: Double)(multiplier: Int): String =
      (value * multiplier).toString

    val x = 10.4
    val y = 3
    f(x)(y) shouldEqual uncurry(f)(x, y)
  }

  "compose" should "関数を合成する" in {
    def f(s: String) = s + s

    def g(n: Int): String = n.toString

    compose(f, g)(2) shouldBe "22"
  }
}
