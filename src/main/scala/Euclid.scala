import scala.annotation.tailrec

object Euclid {
  /**
    * Euclid's algorithm
    * @param a <Int>
    * @param b <Int>
    * @return greatest common divisor
    */
   @tailrec
   private[this] def solve(a: Int, b: Int): Int = if (b == 0) a else solve(b, a%b)
   def gcd(a: Int, b: Int) = solve(a, b)
}
