package chapter4

import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary._
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

class EitherTest extends WordSpec with GeneratorDrivenPropertyChecks {

  implicit def leftArb[E](implicit e: Arbitrary[E]): Arbitrary[Left[E]] = Arbitrary(Gen.resultOf[E, Left[E]](Left(_)))

  implicit def rightArb[A](implicit a: Arbitrary[A]): Arbitrary[Right[A]] = Arbitrary(Gen.resultOf[A, Right[A]](Right(_)))

  implicit def eitherArb[E, A](implicit e: Arbitrary[E], a: Arbitrary[A]): Arbitrary[Either[E, A]] = Arbitrary(Gen.oneOf(arbitrary[Left[E]], arbitrary[Right[A]]))

  "map" should {
    "Leftなら変わらない" in
      forAll { (l: Left[String], f: Int => Double) =>
        l.map(f) shouldBe l
      }

    "Rightなら中身に関数適用" in
      forAll { (r: Right[Int], f: Int => Double) =>
        r.map(f) shouldBe Right(f(r.value))
      }
  }

  "flatMap" should {
    "Leftなら変わらない" in
      forAll { (l: Left[String], f: Int => Either[String, Double]) =>
        l.flatMap(f)
      }

    "Rightなら中身に関数適用" in
      forAll { (r: Right[Int], f: Int => Either[String, Double]) =>
        r.flatMap(f) shouldBe f(r.value)
      }
  }

  "orElse" should {
    "thisがRightならthis" in
      forAll { (r: Right[Int], other: Either[String, Int]) =>
        r.orElse(other) shouldBe r
      }
    "thisがLeftなら引数の値" in
      forAll { (l: Left[Int], other: Either[String, Int]) =>
        l.orElse(other) shouldBe other
      }
  }

  "map2" should {
    "両方ともRightなら関数を適用" in {
      forAll { (r1: Right[Int], r2: Right[String], f: (Int, String) => Double) =>
        r1.map2(r2)(f) shouldBe Right(f(r1.value, r2.value))
      }
    }

    "thisがLeftならthis" in {
      forAll { (l: Left[String], e2: Either[String, Int], f: (Double, Int) => String) =>
        l.map2(e2)(f) shouldBe l
      }
    }

    "thisがRightでotherがLeftならother" in {
      forAll { (r: Right[Double], l: Left[String], f: (Double, Int) => String) =>
        r.map2(l)(f) shouldBe l
      }
    }
  }

  "sequence" should {
    "すべてRightならばRight(List(a, b, c, ...))のようになる" in
      forAll { (rights: List[Right[Int]]) =>
        Either.sequence(rights) shouldBe Right(rights.map(_.value))
      }

    "Leftを含むなら最初のLeft" in
      forAll { (preffix: List[Right[Int]], suffix: List[Either[String, Int]], message: String) =>
        Either.sequence(preffix ++ (Left(message) :: suffix)) shouldBe Left(message)
      }
  }

  "traverse" should {
    "すべてRightを返すならRight(List(a, b, c, ...))のようになる" in
      forAll { (a: List[Int], f: Int => Right[String]) =>
        Either.traverse(a)(f) shouldBe Right(a.map(f).map(_.value))
      }

    "Listが空ならRight(List())" in
      forAll { (f: Int => Either[String, Double]) =>
        Either.traverse(List())(f) shouldBe Right(List())
      }

    "Leftを含むならば最初のLeft" in
      forAll {
        (preffix: List[Int], N: Int, suffix: List[Int], fail: Int => Left[String], success : Int => Right[Double] , any: Int => Either[String, Double]) =>
          val nIsFirstLeft: PartialFunction[Int, Either[String, Double]] = {
            case N => fail(N)
            case n if preffix.contains(n) => success(n)
          }

          Either.traverse(preffix ++ (N :: suffix))(nIsFirstLeft.orElse(PartialFunction(any))) shouldBe fail(N)
      }
  }
}
