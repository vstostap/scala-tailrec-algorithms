import scala.annotation.tailrec

object Factorial {
  /**
    * Tail-recursive factorial
    * @param x <Int> value
    * @return factorial of value
    */
  private[this] def solve(x: Int) = {
    @tailrec
    def iter(x: Int, acc: Int): Int =
      if (x == 0) 0
      else if (x == 1) acc
      else iter(x-1, acc*x)
    iter(x, 1)
  }
  def factorial(x: Int) = solve(x)
}
