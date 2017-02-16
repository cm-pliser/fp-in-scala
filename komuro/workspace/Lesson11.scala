case class Point(x: Int, y: Int)

// DayOfWeek
sealed abstract class DayOfWeek
case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek


// Tree
sealed abstract class Tree
case class Branch(value: Int, left: Tree, right: Tree) extends Tree
case object Empty extends Tree


// Execute Code
object Lesson11 {

  def nextDayOfWeek(d: DayOfWeek): DayOfWeek = d match {
    case Sunday => Monday
    case Monday => Tuesday
    case Tuesday => Wednesday
    case Wednesday => Thursday
    case Thursday => Friday
    case Friday => Saturday
    case Saturday => Sunday
  }

  def max(tree: Tree): Int = tree match {
    case Branch(v, l, r) => List(v, max(l), max(r)).max
    case Empty => 0
  }

  def max_2(tree: Tree): Int = tree match {
    case Branch(v, Empty, Empty) => {
      v
    }
    case Branch(v, left, Empty) => {
      val lMax = max(left)
      if (lMax > v) lMax
      else v
    }
    case Branch(v, Empty, right) => {
      val rMax = max(right)
      if (rMax > v) rMax
      else v
    }
    case Branch(v, left, right) => {
      val lMax = max(left)
      val rMax = max(right)
      if (v > lMax) {
        if (v > rMax) v else rMax
      } else {
        if (lMax > rMax) lMax else rMax
      }
    }
    case Empty => {
      throw new Exception
    }
  }
  def min(tree: Tree): Int = ???
  def depth(tree: Tree): Int = ???
}
