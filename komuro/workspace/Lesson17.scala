object Lesson17 {
    implicit def stringToBoolean(str: String): Boolean = 
    str.toLowerCase() match {
        case "hoge" => true
        case _ => false
    }

    def main(args: Array[String]): Unit = {
        if ("Hoge") {
            println("ヤバみ")
        } else {
            println("尊み")
        }
    }
}