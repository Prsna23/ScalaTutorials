import math.abs

object FixedPoint {
  val tolerance = 0.00001

  def isClose(x : Double, y : Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f : Double => Double) (guess : Double) = {
    def iterate(guess : Double) : Double = {
      val next = f(guess)
      if(isClose(guess, next)) next
      else iterate(next)
    }
    iterate(guess)
  }

  fixedPoint(x => 1 + x / 2) (1.0)

  def avgDamp(f : Double => Double)(x : Double) = (x + f(x)) / 2

  def sqrt(x : Double) = fixedPoint(avgDamp(y => x / y))(1)

  sqrt(2)
}