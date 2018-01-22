package chapter4

import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary._
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

class EitherTest extends WordSpec with GeneratorDrivenPropertyChecks {

  implicit def leftArb[E](implicit e: Arbitrary[E]): Arbitrary[Ior.Left[E]] = Arbitrary(Gen.resultOf[E, Ior.Left[E]](Ior.Left(_)))

  implicit def rightArb[A](implicit a: Arbitrary[A]): Arbitrary[Ior.Right[A]] = Arbitrary(Gen.resultOf[A, Ior.Right[A]](Ior.Right(_)))

  implicit def eitherArb[E, A](implicit e: Arbitrary[E], a: Arbitrary[A]): Arbitrary[Either[E, A]] = Arbitrary(Gen.oneOf(arbitrary[Ior.Left[E]], arbitrary[Ior.Right[A]]))

  "map" should {
    "Leftなら変わらない" in
      forAll { (l: Ior.Left[String], f: Int => Double) =>
        l.map(f) shouldBe l
      }

    "Rightなら中身に関数適用" in
      forAll { (r: Ior.Right[Int], f: Int => Double) =>
        r.map(f) shouldBe Ior.Right(f(r.value))
      }
  }

  "flatMap" should {
    "Leftなら変わらない" in
      forAll { (l: Ior.Left[String], f: Int => Either[String, Double]) =>
        l.flatMap(f)
      }

    "Rightなら中身に関数適用" in
      forAll { (r: Ior.Right[Int], f: Int => Either[String, Double]) =>
        r.flatMap(f) shouldBe f(r.value)
      }
  }

  "orElse" should {
    "thisがRightならthis" in
      forAll { (r: Ior.Right[Int], other: Either[String, Int]) =>
        r.orElse(other) shouldBe r
      }
    "thisがLeftなら引数の値" in
      forAll { (l: Ior.Left[Int], other: Either[String, Int]) =>
        l.orElse(other) shouldBe other
      }
  }

  "map2" should {
    "両方ともRightなら関数を適用" in {
      forAll { (r1: Ior.Right[Int], r2: Ior.Right[String], f: (Int, String) => Double) =>
        r1.map2(r2)(f) shouldBe Ior.Right(f(r1.value, r2.value))
      }
    }

    "thisがLeftならthis" in {
      forAll { (l: Ior.Left[String], e2: Either[String, Int], f: (Double, Int) => String) =>
        l.map2(e2)(f) shouldBe l
      }
    }

    "thisがRightでotherがLeftならother" in {
      forAll { (r: Ior.Right[Double], l: Ior.Left[String], f: (Double, Int) => String) =>
        r.map2(l)(f) shouldBe l
      }
    }
  }

  "sequence" should {
    "すべてRightならばRight(List(a, b, c, ...))のようになる" in
      forAll { (rights: List[Ior.Right[Int]]) =>
        Either.sequence(rights) shouldBe Ior.Right(rights.map(_.value))
      }

    "Leftを含むなら最初のLeft" in
      forAll { (preffix: List[Ior.Right[Int]], suffix: List[Either[String, Int]], message: String) =>
        Either.sequence(preffix ++ (Ior.Left(message) :: suffix)) shouldBe Ior.Left(message)
      }
  }

  "traverse" should {
    "すべてRightを返すならRight(List(a, b, c, ...))のようになる" in
      forAll { (a: List[Int], f: Int => Ior.Right[String]) =>
        Either.traverse(a)(f) shouldBe Ior.Right(a.map(f).map(_.value))
      }

    "Listが空ならRight(List())" in
      forAll { (f: Int => Either[String, Double]) =>
        Either.traverse(List())(f) shouldBe Ior.Right(List())
      }

    "Leftを含むならば最初のLeft" in
      forAll {
        (preffix: List[Int], N: Int, suffix: List[Int], fail: Int => Ior.Left[String], success : Int => Ior.Right[Double], any: Int => Either[String, Double]) =>
          val nIsFirstLeft: PartialFunction[Int, Either[String, Double]] = {
            case N => fail(N)
            case n if preffix.contains(n) => success(n)
          }

          Either.traverse(preffix ++ (N :: suffix))(nIsFirstLeft.orElse(PartialFunction(any))) shouldBe fail(N)
      }
  }
}
