/**
  * @author vstostap
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
  private[this] def gcdScope(a: T, b: T): T = if (b == 0.0) a else gcdScope(b, a % b)
  def gcd(a: T, b: T): T = gcdScope(a, b)

  /**
    * Isaac Newton's sqrt algorithm
    * @param x <T> value
    * @return square root of value
    */
  private[this] def sqrtScope(x: T): T = {
    def square(x: T): T = x * x
    def good_enough(guess: T) = (square(guess) - x).abs < 0.001
    def improve(guess: T) = (guess + x / guess) / 2
    @tailrec
    def iter(guess: T): T =
      if (good_enough(guess)) guess
      else iter(improve(guess))
    iter(1.0)
  }
  def sqrt(x: T): T = sqrtScope(x)

  /**
    * Tail-recursive factorial
    * @param x <T> value
    * @return factorial of value
    */
  private[this] def factorialScope(x: T) = {
    @tailrec
    def iter(x: T, acc: T): T =
      if (x == 0.0) 0
      else if (x == 1.0) acc
      else iter(x - 1, acc * x)
    iter(x, 1)
  }
  def factorial(x: T): T = factorialScope(x)

  /**
    * Fibonacci numbers
    * @param n <T>
    * @return
    */
  private[this] def fibScope(n: T): T = {
    @tailrec
    def iter(n: T, a: T, b: T): T = n match {
      case 0 => a
      case _ => iter(n - 1, b, a + b)
    }
    iter(n, 0, 1)
  }
  def fib(n: T): T = fibScope(n)

  /**
    * Search fixed-point functions
    * @param f <T> => <T>
    * @param firstGuess <T>
    * @return fixed-point
    */
  private[this] def fixedPointScope(f: T => T)(firstGuess: T) = {
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
  def fixedPoint(f: T => T)(firstGuess: T) = fixedPointScope(f)(firstGuess)

  /**
    * Tail-recursive sum of functions
    * @param f <T> => <T> function
    * @param nums (<T>, <T>) interval
    * @return sum of functions from each number on interval
    */
  private[this] def sumOfFuncsScope(f: T => T)(nums: (T, T)): T = {
    val order : (T, T) = if(nums._1 < nums._2) (nums._1, nums._2) else (nums._2, nums._1)
    @tailrec
    def iter(less: T, acc: T): T = {
      if (less > order._2) acc
      else iter(less + 1, acc + f(less))
    }
    iter(order._1, 0)
  }
  def sumOfFuncs(f: T => T)(nums: (T, T)) = sumOfFuncsScope(f)(nums)


  /**
    * Tail-recursive product of functions
    * @param f <T> => <T> function
    * @param nums (<T>, <T>) interval
    * @return product of functions from each number on interval
    */
  private[this] def productOfFuncsScope(f: T => T)(nums: (T, T)): T = {
    val order : (T, T) = if(nums._1 < nums._2) (nums._1, nums._2) else (nums._2, nums._1)
    @tailrec
    def iter(less: T, acc: T): T = {
      if (less > order._2) acc
      else iter(less + 1, acc * f(less))
    }
    iter(order._1, 1)
  }
   def productOfFuncs(f: T => T)(nums: (T, T)) = productOfFuncsScope(f)(nums)

  /**
    * Triangular numbers
    * @param n <T>
    * @return sum of triangular numbers
    */
  private[this] def triangularNumbersScope(n:T) : T = {
    @tailrec
    def iter(n: T, acc: T): T = {
      if (n <= 1) acc
      else iter(n - 1, n + acc)
    }
    iter(n, 1)
  }
  def triangularNumbers(n:T) = triangularNumbersScope(n)
}