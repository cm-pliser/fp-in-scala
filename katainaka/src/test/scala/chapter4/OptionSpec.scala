package chapter4

import org.scalacheck.{Arbitrary, Gen}, Arbitrary._
import org.scalatest.{FlatSpec, MustMatchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class OptionSpec
    extends FlatSpec
    with GeneratorDrivenPropertyChecks
    with MustMatchers {

  implicit def arbSome[A](implicit arbA: Arbitrary[A]): Arbitrary[Some[A]] =
    Arbitrary(Gen.resultOf(Some[A](_)))

  implicit def arbOption[A](implicit arbA: Arbitrary[A]): Arbitrary[Option[A]] =
    Arbitrary(Gen.oneOf(arbitrary[Some[A]], Gen.const(None)))

  "map" should "Someの中身に関数を適用する" in
    forAll { (some: Some[Int], f: Int => String) =>
      some.map(f) mustBe Some(f(some.value))
    }

  "map" should "Noneにはなにもしない" in
    forAll { (f: Int => String) =>
      None.map(f) mustBe None
    }

  "flatMap" should "Someに対しては中身を取り出して関数適用" in
    forAll { (some: Some[Int], f: Int => Option[String]) =>
      some.flatMap(f) mustBe f(some.value)
    }

  "flatMap" should "Noneに対してはNone" in
    forAll { (f: Int => Option[String]) =>
      None.flatMap(f) mustBe None
    }

  "getOrElse" should "Someに対しては中身を返す" in
    forAll { (some: Some[Int], j: Int) =>
      some.getOrElse(j) mustBe some.value
    }

  "getOrElse" should "Noneに対しては引数" in
    forAll { (j: Int) =>
      None.getOrElse(j) mustBe j
    }

  "orElse" should "Someに対しては元の値" in
    forAll { (some: Some[Int], maybeJ: Option[Int]) =>
      some.orElse(maybeJ) mustBe some
    }

  "orElse" should "Noneに対しては引数" in
    forAll { (maybeJ: Option[Int]) =>
      None.orElse(maybeJ) mustBe maybeJ
    }

  "filter" should "Someで中身に対して関数を適用するとtrueが返る時はそのまま" in
    forAll { (some: Some[Int]) =>
      some.filter(_ => true) mustBe some
    }
  "filter" should "Someで中身に対して関数を適用するとfalseが返る時はNone" in
    forAll { (some: Some[Int]) =>
      some.filter(_ => false) mustBe None
    }
  "filter" should "Noneの時はNone" in
    forAll { (f: Int => Boolean) =>
      None.filter(f) mustBe None
    }

  "map2" should "Some,Someの時は中身に関数を適用" in
    forAll { (maybeA: Some[Int], maybeB: Some[String], f: (Int, String) => Double) =>
      Option.map2(maybeA, maybeB)(f) mustBe Some(f(maybeA.value, maybeB.value))
    }

  "map2" should "1番目の引数がNoneであればNone" in
    forAll { (maybeB: Option[String], f: (Int, String) => Double) =>
      Option.map2(None, maybeB)(f) mustBe None
    }

  "map2" should "2番目の引数がNoneであればNone" in
    forAll { (maybeA: Option[Int], f: (Int, String) => Double) =>
      Option.map2(maybeA, None)(f) mustBe None
    }

  "sequence" should "すべてSomeならばSome(List(a, b, c, ...))のようになる" in
    forAll { (somes: List[Some[Int]]) =>
      Option.sequence(somes) mustBe Some(somes.map(_.value))
    }

  "sequence" should "Noneを含むならばNone" in
    forAll { (preffix: List[Option[Int]], suffix: List[Option[Int]]) =>
      Option.sequence(preffix ++ (None :: suffix)) mustBe None
    }

  "traverse" should "すべてSomeを返すならSome(List(a, b, c, ...))のようになる" in
    forAll { (a: List[Int], f: Int => Some[String]) =>
      Option.traverse(a)(f) mustBe Some(a.map(f).map(_.value))
    }

  "traverse" should "Listが空ならSome(List())" in
    forAll { (f: Int => Option[String]) =>
      Option.traverse(List())(f) mustBe Some(List())
    }

  "traverse" should "Noneを含むならばNone" in
    forAll { (preffix: List[Int], N: Int, suffix: List[Int], f: Int => Option[String]) =>
      val g: PartialFunction[Int, Option[String]] = { case N => None }

      Option.traverse(preffix ++ (N :: suffix))(g.orElse(PartialFunction(f))) mustBe None
    }
}
