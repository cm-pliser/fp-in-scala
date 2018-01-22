package chapter4

object Variance {
  def variance(xs: Seq[Double]): Option[Double] = {
    lazy val sum = xs.sum
    lazy val count = xs.length
    xs match {
      case xs@(_ +: _) => {
        val mean = sum / count
        val vs =  xs.map(x => math.pow(x - mean, 2))
        Some(vs.sum/vs.length)
      }
      case _ => None
    }
  }

}
