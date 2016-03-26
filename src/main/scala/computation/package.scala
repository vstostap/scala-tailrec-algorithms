/**
  * @author vstostap
  * @see https://github.com/vstostap/scala-tailrec-algorithms
  */

import scala.annotation.tailrec

package object computation {

  type T = Double

  /**
    * Is power of two
    * @param a <Int>
    * @return <Boolean> whether the given number is power of two.
    * Complexity O(1)
    */
  def isPowerOfTwo(a: Int): Boolean =
    (a & (a - 1)) == 0

  /**
    * Swaps even and odd bits in given number
    * @param a <Int>
    * @return number
    * Complexity O(1)
    */
  def swapEvenAndOdd(a: Int): Int =
    ((a & 0xaaaaaaaa) >> 1) | ((a & 0x55555555) << 1)

  /**
    * Maximum of two numbers
    * @param a <Int>
    * @param b <Int>
    * @return number
    * Complexity O(1)
    */
  def max(a: Int, b: Int) = a - (((a - b) >> 31) & 0x1) * (a - b)

  /**
    * Euclid's algorithm
    * @param a <T>
    * @param b <T>
    * @return greatest common divisor
    * Complexity O(log n)
    */
  @tailrec
  private[this] def gcdScope(a: T, b: T): T = if (b == 0.0) a else gcdScope(b, a % b)
  def gcd(a: T, b: T): T = gcdScope(a, b)

  /**
    * Least common multiple
    * @param a <T>
    * @param b <T>
    * @return lcm of two given numbers
    * Complexity O(log n)
    */
  private[this] def lcmScope(a: T, b: T): T = math.abs(a * b) / gcd(a, b)
  def lcm(a: T, b: T): T = lcmScope(a, b)

  /**
    * Isaac Newton's sqrt algorithm
    * @param x <T> value
    * @return square root of value
    * Complexity O(log n)
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
    * Exponentiation of given number 'a' in power of 'b'
    * @param a <T>
    * @param b <T>
    * @return 'a' in power of 'b'
    * Complexity O(log n)
    */
  private[this] def powerScope(a: T, b: T): Double = {
    @tailrec
    def iter(x: T, acc: T, y: T): T =
      if (y == 0.0) acc
      else if (y % 2 == 0) iter(x * x, acc, y / 2)
      else iter(x, acc * x, y - 1)

    iter(a, 1.0, b)
  }
  def power(a: T, b: T) = powerScope(a, b)

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
    * @return 'n'th fibonacci number
    * Complexity O(n)
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


  /**
    * Pascal triangle
    * @param n <T>
    * @return prints the `n`th levels of Pascal's Triangle.
    */
  private[this] def pascalTriangle(n: T): Unit = {
    def pascal(i: Int, j: Int): Int =
      if (j == 0 || j == i) 1
      else pascal(i - 1, j - 1) + pascal(i - 1, j)

    @tailrec
    def iter(m: Int): Unit =
      if (m < n) {
        for (k <- 0 to m) print(pascal(m, k) + " ")
        print("\n")
        iter(m + 1)
      } else print("\n")

    iter(0)
  }
  def pascal(n: T) = pascalTriangle(n)


  /**
    * Validate parenthesis
    * @param s <String>
    * @return <Boolean> whether the parenthesis are balanced or not
    */
  private[this] def validateParenthesisScope(s: String): Boolean = {
    def left(i: Int, k: Int): Boolean =
      if (i == s.length) k == 0
      else if (s.charAt(i) == '(') right(i + 1, k + 1)
      else false

    @tailrec
    def right(i: Int, k: Int): Boolean =
      if (i == s.length) false
      else if (s.charAt(i) == '(') right(i + 1, k + 1)
      else if (k == 1) left(i + 1, k - 1)
      else right(i + 1, k - 1)

    left(0, 0)
  }
  def validateParenthesis(s: String): Boolean = validateParenthesisScope(s)
}