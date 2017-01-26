/**
 * Created by suwa-yuki on 2017/01/26.
 */
object Main {

  def main(args: Array[String]): Unit = {

    // ケースクラス
    sealed abstract class DayOfWeek
    case object Sunday extends DayOfWeek
    case object Monday extends DayOfWeek
    case object Tuesday extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday extends DayOfWeek
    case object Friday extends DayOfWeek
    case object Saturday extends DayOfWeek

    // 練習問題
    def nextDayOfWeek(d: DayOfWeek): DayOfWeek = d match {
      case Sunday => Monday
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Sunday
    }

    println(nextDayOfWeek(Sunday))

    // 練習問題
    sealed abstract class Tree
    case class Branch(value: Int, left: Tree, right: Tree) extends Tree
    case object Empty extends Tree

    def max(t: Tree): Int = t match {
      case Branch(v, Empty, Empty) => v
      case Branch(v, Empty, r) =>
        val x = max(r)
        if(v > x) v else x
      case Branch(v, l, Empty) =>
        val x = max(l)
        if(v > x) v else x
      case Branch(v, l, r) =>
        val x = max(l)
        val y = max(r)
        if(v > x) {
          if(v > y) v else y
        } else {
          if(x > y) x else y
        }
      case Empty =>
        throw new RuntimeException
    }

    def maxList(t: Tree): Int = t match {
      case Branch(v, l, r) => List(v, max(l), max(r)).max
      case Empty => 0
    }

    def min(t: Tree): Int = t match {
      case Branch(v, Empty, Empty) => v
      case Branch(v, Empty, r) =>
        val x = min(r)
        if(v < x) v else x
      case Branch(v, l, Empty) =>
        val x = min(l)
        if(v < x) v else x
      case Branch(v, l, r) =>
        val x = min(l)
        val y = min(r)
        if(v < x) {
          if(v < y) v else y
        } else {
          if(x < y) x else y
        }
      case Empty =>
        throw new RuntimeException
    }

    def depth(t: Tree): Int = t match {
      case Empty => 0
      case Branch(_, l, r) => depth(l).max(depth(r)) + 1
    }

    val tree: Tree = Branch(1, Branch(2, Branch(2, Empty, Empty), Empty), Branch(3, Empty, Branch(4, Empty, Empty)))
    println("max = " + max(tree))
    println("maxList = " + maxList(tree))
    println("min = " + min(tree))
    println("depth = " + depth(tree))

    def toList(tree: Tree): List[Int] = tree match {
      case Empty => Nil
      case Branch(v, l, r) => toList(l) ++ List(v) ++ toList(r)
    }

    def sort(t: Tree): Tree = {
      def fromList(list: List[Int]): Tree = {
        def insert(value: Int, t: Tree): Tree = t match {
          case Empty => Branch(value, Empty, Empty)
          case Branch(v, l, r) =>
            if(value <= v) Branch(v, insert(value, l), r)
            else Branch(v, l, insert(value, r))
        }
        list.foldLeft(Empty:Tree){ case (t, v) => insert(v, t) }
      }
      fromList(toList(t))
    }

    println("sort = " + sort(tree))

  }

}
