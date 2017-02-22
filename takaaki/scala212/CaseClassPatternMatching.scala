/**
  * https://dwango.github.io/scala_text/case-class-and-pattern-matching.html
  */

sealed abstract class DayOfWeek
case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek

object CaseClassPatternMatching {
  def main(args: Array[String]): Unit = {
    val d = nextDayOfWeek(Thursday)
    print(d)
  }

  def nextDayOfWeek(d: DayOfWeek): DayOfWeek = d match {
    case Sunday => Monday
    case Monday => Tuesday
    case Tuesday => Wednesday
    case Wednesday => Thursday
    case Thursday => Friday
    case Friday => Saturday
    case Saturday => Sunday
  }

  object BinaryTree {
    sealed abstract class Tree
    case class Branch(value: Int, left: Tree, right: Tree) extends Tree
    case object Empty extends Tree

    def max(t: Tree): Int = t match {
      case Branch(v, Empty, Empty) =>
        v
      case Branch(v, Empty, r) =>
        val x = max(r)
        if (v > x) v else x
      case Branch(v, l, Empty) =>
        val x = max(l)
        if (v > x) v else x
      case Branch(v, l, r) =>
        val x = max(l)
        val y = max(r)
        if (v > x) {
          if (v > y) v else y
        } else {
          if (x > y) x else y
        }
      case Empty =>
        throw new RuntimeException
    }

    def min(t: Tree): Int = t match {
      case Branch(v, Empty, Empty) =>
        v
      case Branch(v, Empty, r) =>
        val x = min(r)
        if (v < x) v else x
      case Branch(v, l, Empty) =>
        val x = min(l)
        if (v < x) v else x
      case Branch(v, l, r) =>
        val x = min(l)
        val y = min(r)
        if (v < x) {
          if(v < y) v else y
        } else {
          if(x < y) x else y
        }
      case Empty =>
        throw new RuntimeException
    }

    def depth(t: Tree): Int = t match {
      case Empty => 0
      case Branch(_, l, r) =>
        val ldepth = depth(l)
        val rdepth = depth(r)
        (if (ldepth < rdepth) rdepth else ldepth) + 1
    }

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

    def find(t: Tree, target: Int): Boolean = t match {
      case Branch(v, l, r) => if (v == target) true else (find(l, target) || find(r, target))
      case Empty => false
    }

    def findBinaryTree(t: Tree, target: Int): Boolean = t match {
      case Branch(v, l, r) => if (v == target) true else (if  (target <= v) findBinaryTree(l, target) else findBinaryTree(r, target))
      case Empty => false
    }
  }
}
