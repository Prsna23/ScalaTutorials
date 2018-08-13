object SquareRootCalculator{
  def abs(x : Double) = if(x >= 0) x else -x
  def sqrt(x : Double) = {
    def sqrtIter(est: Double): Double =
      if (isGoodEnough(est)) est
      else sqrtIter(improve(est))

    def isGoodEnough(est: Double) =
      abs(est * est - x) / x < 0.00001

    def improve(est: Double) =
      (est + x / est) / 2

    sqrtIter(1.0)
  }
  sqrt(4)
}