package chapter4

import chapter4.Ior.{ Left, Right }
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class MeanTest extends FlatSpec with GeneratorDrivenPropertyChecks{
  "mean" should "空のリストにはLeft" in {
    Mean.mean(IndexedSeq()) shouldBe Left("mean of empty list!")
  }

  "mean" should "空でないリストにはRight(合計/個数)" in
    forAll(Gen.nonEmptyListOf(arbitrary[Double])) {
      (list) => Mean.mean(list.toIndexedSeq) shouldBe Right(list.sum/list.length)
    }
}
