object state {

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

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(c: Int, gen: RNG, acc: List[Int]): (List[Int], RNG) = c match {
      case 0 => (acc, gen)
      case _ =>
        val (v, nextGen) = gen.nextInt
        loop(c - 1, nextGen, v :: acc)
    }

    loop(count, rng, Nil)
  }
}
