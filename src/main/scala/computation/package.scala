/**
  * @author Ostap Vasiuta
  * @see https://github.com/vstostap/scala-tailrec-algorithms
  */

import scala.annotation.tailrec

package object computation {

  type T = Double

  /**
    * Euclid's algorithm
    * @param a <T>
    * @param b <T>
    * @return greatest common divisor
    */
  @tailrec
  private[this] def gcd_scope(a: T, b: T): T = if (b == 0.0) a else gcd_scope(b, a % b)
  def gcd(a: T, b: T): T = gcd_scope(a, b)

  /**
    * Isaac Newton's sqrt algorithm
    * @param x <T> value
    * @return square root of value
    */
  private[this] def sqrt_scope(x: T): T = {
    def square(x: T): T = x * x
    def good_enough(guess: T) = (square(guess) - x).abs < 0.001
    def improve(guess: T) = (guess + x / guess) / 2
    @tailrec
    def iter(guess: T): T =
      if (good_enough(guess)) guess
      else iter(improve(guess))
    iter(1.0)
  }
  def sqrt(x: T): T = sqrt_scope(x)

  /**
    * Tail-recursive factorial
    * @param x <T> value
    * @return factorial of value
    */
  private[this] def factorial_scope(x: T) = {
    @tailrec
    def iter(x: T, acc: T): T =
      if (x == 0.0) 0
      else if (x == 1.0) acc
      else iter(x - 1, acc * x)
    iter(x, 1)
  }
  def factorial(x: T): T = factorial_scope(x)

  /**
    * Fibonacci numbers
    * @param n <T>
    * @return
    */
  private[this] def fib_scope(n: T): T = {
    @tailrec
    def iter(n: T, a: T, b: T): T = n match {
      case 0 => a
      case _ => iter(n - 1, b, a + b)
    }
    iter(n, 0, 1)
  }
  def fib(n: T): T = fib_scope(n)

  /**
    * Search fixed-point functions
    * @param f <T> => <T>
    * @param firstGuess <T>
    * @return fixed-point
    */
  private[this] def fixedPoint_scope(f: T => T)(firstGuess: T) = {
    val tolerance = 0.0001
    def isCloseEnough(x: T, y: T) = Math.abs((x - y) / x) < tolerance
    @tailrec
    def iterate(guess: T): T = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  def fixedPoint(f: T => T)(firstGuess: T) = fixedPoint_scope(f)(firstGuess)

}