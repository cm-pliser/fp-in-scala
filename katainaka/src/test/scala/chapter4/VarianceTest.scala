package chapter4

import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class VarianceTest extends FlatSpec with GeneratorDrivenPropertyChecks {
  "variance" should "空のSeqにはNoneを返す" in {
    Variance.variance(Seq.empty) shouldBe None
  }

  "variance" should "空でないSeqにはSomeと平均を返す" in
    forAll(Gen.nonEmptyListOf(Gen.choose(-10000.0, 10000.0))) { (seq: Seq[Double]) =>

      val double = seq.map(x => x * x)

      Variance.variance(seq) match {
        case Some(v) => v shouldBe (double.sum / double.length - math.pow(seq.sum / seq.length, 2)) +- (.001 * seq.length)
        case None => fail()
      }
    }
}
