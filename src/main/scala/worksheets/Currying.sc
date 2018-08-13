object Currying{
  def product(f : Int => Int)(a : Int, b : Int) : Int =
    reduce(x => x, (x, y) => x * y, 1) (a, b)
  product(x => x) (3, 5)

  def fact(n : Int) = product(x => x) (1, n)

  fact(5)

  def reduce(f : Int => Int, combine : (Int, Int) => Int, zero : Int) (a : Int, b : Int) : Int =
    if(a > b) zero
    else combine(f(a), reduce(f, combine, zero)(a + 1, b))

  reduce(x => x, (x, y) => x * y, 1) (3, 5)
}