package chapter4

object Mean {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) {
      Left("mean of empty list!")
    } else {
      Right(xs.sum/ xs.length)
    }
  }
}
