object Operation {
  def main(args: Array[String]): Unit = {

    // 構文
    { println("## 構文 ##") }
    { println("A"); println("B"); println(1 + 2) }

    // if式
    { println("## if式 ##") }
    val yang = 17
    if (yang < 18) {
      println("18歳未満です")
    } else {
      println("18歳以上です")
    }

    val old = 82
    if (old < 18) {
      println("18歳未満です")
    } else {
      println("18歳以上です")
    }

    val age = 5
    val isSchoolStarted = false
    if (1 <= age && age <= 6 && !isSchoolStarted) {
      println("幼児です")
    } else {
      println("幼児ではありません")
    }

    // while式
    { println("## while式 ##") }
    var i = 1
    while (i <= 10) {
      println("i = " + i)
      i = i + 1
    }

    var j = 0
    do {
      println(j)
      j += 1
    } while (j < 10)

    // for式
    for (x <- 1 to 5; y <- 1 until 5) {
      println("x = " + x + " y = " + y)
    }

    for (x <- 1 to 5; y <- 1 until 5 if x != y) {
      println("x = " + x + " y = " + y)
    }

    for (e <- List("A", "B", "C", "D", "E")) println(e)

    val forList = for (e <- List("A", "B", "C", "D", "E")) yield {
      "Pre" + e
    }
    { println(forList) }

    for (a <- 1 to 100; b <- 1 to 100; c <- 1 to 100 if a * a == b * b + c * c) {
      println((a, b, c))
    }

    // match式
    { println("## match式 ##") }

    val taro = "Taro"
    taro match {
      case "Taro" => println("Male")
      case "Jiro" => println("Male")
      case "Hanako" => println("Female")
    }

    val one = 1
    one match {
      case 1 => println("one")
      case 2 => println("two")
      case _ => println("other")
    }

    "abc" match {
      case "abc" => println("first")   // ここで処理が終了
      case "def" => println("second") // こっちは表示されない
    }

    "abc" match {
      case "abc" | "def" =>
        println("first")
        println("second")
    }

    val lst = List("A", "B", "C", "D", "E")
    lst match {
      case List("A", b, c, d, e) =>
        println("b = " + b)
        println("c = " + c)
        println("d = " + d)
        println("e = " + e)
      case _ =>
        println("nothing")
    }

    lst match {
      case List("A", b, c, d, e) if b != "B" =>
        println("b = " + b)
        println("c = " + c)
        println("d = " + d)
        println("e = " + e)
      case _ =>
        println("nothing")
    }

    val list = List(List("A"), List("B", "C", "D", "E"))
    list match {
      case List(a@List("A"), x) =>
        println(a)
        println(x)
      case _ => println("nothing")
    }

    (List("a"): Any) match {
      case List(_) | Some(_) =>
        println("ok")
    }

  }
}
