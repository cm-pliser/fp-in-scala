import scala.collection.JavaConverters._
import java.util._
import scala.collection.mutable.ArrayBuffer

class JavaAndScala {

  /**
   * nullとOption
   */
  def optionSample(): Unit = {
    val map = new java.util.HashMap[String, Int]()
    map.put("A", 1)
    map.put("B", 2)
    map.put("C", 3)

    println("OptionSample=======>")
    val maybeA = Option(map.get("A"))
    println(s"$maybeA")

    val maybeB = Option(map.get("B"))
    println(s"$maybeB")

    val maybeC = Option(map.get("C"))
    println(s"$maybeC")

    val maybeD = Option(map.get("D"))
    println(s"$maybeD")
  }

  /**
   * JavaConverters
   */
  def converterSample(): Unit = {
    val list = new ArrayList[String]()
    list.add("A")
    list.add("B")
    val scalaList = list.asScala

    println("ConverterSample=======>")
    println(s"$scalaList")
  }

  /**
   * 演習問題1
   */
  def practiceCollection(): Unit = {
    val arrayBuffer = new ArrayBuffer[String]
    arrayBuffer.append("Hogehoge")

    println("JavaConverter=======>")

    val javaList = arrayBuffer.asJava
    // val javaList = arrayBuffer
    javaList match {
      case _:java.util.List[String] => println("Javaのリストです")
      case _ => println("お前だれや。知らんやつや")
    }
  }
}

object JavaAndScala {

  def main(args: Array[String]):Unit = {
    val hoge = new JavaAndScala()
    // hoge.optionSample()
    // hoge.converterSample()
    // hoge.practiceCollection()
  }
}
