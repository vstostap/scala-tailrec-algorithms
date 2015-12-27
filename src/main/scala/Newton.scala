import scala.annotation.tailrec

object Newton {
  /**
    * Isaac Newton's sqrt algorithm
    * @param x <Double> value
    * @return square root of value
    */
    private[this] def solve(x: Double): Double = {
      def square(x: Double): Double = x*x
      def good_enough(guess: Double) = (square(guess) - x).abs < 0.001
      def improve(guess: Double) = (guess + x / guess) / 2
      @tailrec
      def iter(guess: Double): Double =
        if (good_enough(guess)) guess
        else iter(improve(guess))
      iter(1.0)
    }
    def sqrt(x: Double) = solve(x)
}
