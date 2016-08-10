trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, nextRNG) = rng.nextInt
    num match {
      case num if num < 0 => (-(num + 1), nextRNG)
      case _ => (num, nextRNG)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, nextRNG) = nonNegativeInt(rng)
    (num / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d,r) = double(rng)
    val (d2,r2) = double(r)
    val (d3,r3) = double(r2)
    ((d,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, list: List[Int]): (List[Int], RNG) = count match {
      case count if count == 0 => (list, r)
      case _  => {
        val (i,r2) = r.nextInt
        go(count - 1, r2, i :: list)
      }
    }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def mapDouble: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
}


val (i, r) = new SimpleRNG(99).nextInt
val (ni, r2) = RNG.nonNegativeInt(r)
println(ni)
println(RNG.double(r2)._1)
println(RNG.nonNegativeEven(r))
println(RNG.mapDouble(r))
