import scala.annotation.tailrec

object Fibonacci {
  /**
    * Fibonacci numbers
    * @param n <Int>
    * @return
    */
  private def solve( n : Int) : Int = {
    @tailrec
    def iter( n: Int, a:Int, b:Int): Int = n match {
      case 0 => a
      case _ => iter( n-1, b, a+b )
    }
    iter( n, 0, 1)
  }
  def fib(n : Int)  = solve(n)
}
