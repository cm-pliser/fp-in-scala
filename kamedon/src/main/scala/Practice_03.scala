/**
  * Created by kamei.hidetoshi on 2016/10/13.
  * 第三回Scala勉強会の練習問題
  */
object Practice_03 {
  def main(args: Array[String]): Unit = {

    match_01

    //    //ifの練習問題
    //    for (x <- 1 to 10) {
    //      print(x + "歳は、")
    //      if_01(x)
    //      println("")
    //    }
    //
    //    //whileの練習問題
    //    loopFrom0To9()
    //    println("")
    //
    //    for_01()
    //    println("")

  }

  //ifの練習問題
  def if_01(x: Int): Unit = {
    val age = 5
    val isSchoolStarted: Boolean = false
    if (x <= age && age <= 6 && !isSchoolStarted) {
      print("幼児です")
    } else {
      print("幼児ではありません")
    }
  }

  //whileの練習問題
  def loopFrom0To9(): Unit = {
    var i = 0
    do {
      print(i)
      i = i + 1
    } while (i <= 9)

  }

  //forの練習問題
  def for_01(): Unit = {
    for (x <- 1 to 1000; y <- 1 to 1000; z <- 1 to 1000 if Math.pow(x, 2) + Math.pow(y, 2) == Math.pow(z, 2)) {
      print("x:%d y:%d z:%d のとき、直角三角形".format(x, y, z))
    }

  }

  //matchの練習問題
  def match_01: Unit = {
    var i = 0
    do {
      val strs = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList
      strs match {
        case List(start, _, _, _, end) if start == end =>
          println("%d: %s".format(i, strs.toString()))
          i = i + 1
        case _ =>
      }
    } while (i <= 1000)

  }


}
