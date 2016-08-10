object state {

  object random {

    type Rand[+A] = RNG => (A, RNG)

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

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt 
      (i & Int.MaxValue, rng2)
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

    val int: Rand[Int] = _.nextInt

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    val nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(i => i - i % 2)

    val doubleA: Rand[Double] =
      map(nonNegativeInt)(i => i.toDouble / Int.MaxValue) 

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, doubleA)
    val randDoubleInt: Rand[(Double, Int)] = both(doubleA, int)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

    def intsA(count: Int)(rng: RNG): (List[Int], RNG) = {
      val intRands = List.range(0, count).map(_ => int)
      sequence(intRands)(rng)
    }
  }
}
