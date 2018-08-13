object Factorial{
  def factorial(n : Int) = {
    def loop(res : Int, n : Int) : Int =
      if(n == 0) res
      else loop(res * n, n - 1)

    loop(1, n)
  }
  factorial(4)

  def sum(f : Int => Int, a : Int, b : Int) = {
    def loop(acc: Int, a : Int) : Int =
      if(a > b) acc
      else loop(f(a) + acc, a + 1)
    loop(0, a)
  }

  sum(factorial, 1, 5)
}