package chapter4

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ValidatedSpec extends FlatSpec {
  "Validated" should "Invalid2つをmap2すると両方のエラーを含んだInvalidとなる" in {
    val nameValidated: Validated[String, String] = Invalid(List("名前は必須項目です"))
    val ageValidated: Validated[String, Int] = Invalid(List("年齢は必須項目です"))
    val result = Validated.map2(nameValidated, ageValidated){(name, age) => s"$name ${age}さい"}

    result shouldBe Invalid(List("名前は必須項目です", "年齢は必須項目です"))
  }

  "Validated" should "Valid2つなら大丈夫" in {
    val nameValidated: Validated[String, String] = Valid("やまさき みちひろ")
    val ageValidated: Validated[String, Int] = Valid(23)
    val result = Validated.map2(nameValidated, ageValidated){(name, age) => s"$name ${age}さい"}

    result shouldBe Valid("やまさき みちひろ 23さい")
  }


  "Validated" should "sequenceは全部のエラー" in {
    val `特技1`: Validated[String, String] = Invalid(List("禁止ワードが含まれています"))
    val `特技2`: Validated[String, String] = Invalid(List("人に見せられる内容を入力しましょう"))
    val `特技3`: Validated[String, String] = Invalid(List("今後の社会人生活に負の影響が大きいです"))
    val `特技4`: Validated[String, String] = Valid("Scalaがすきです")

    val result = Validated.sequence(List(`特技1`, `特技2`, `特技3`, `特技4`))

    result shouldBe Invalid(List("禁止ワードが含まれています", "人に見せられる内容を入力しましょう", "今後の社会人生活に負の影響が大きいです"))
  }

  "Validated" should "traverseはsequenceと同様" in {

    val result = Validated.traverse(List(1, 2, 3 ,4, 5, 6)) { i =>
      if (i % 2 == 0) {
        Invalid(List(i))
      } else {
        Valid(i)
      }
    }

    result shouldBe Invalid(List(2, 4, 6))
  }
}
