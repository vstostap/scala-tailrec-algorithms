/**
  * @author vstostap
  * @see https://github.com/vstostap/scala-tailrec-algorithms
  */

import scala.annotation.tailrec


package object search {

  /**
    * Binary search algorithm
    * @param n <Int>
    * @param l <List>
    * @return sought value
    * Complexity O(log n)
    */
  private[this] def binaryScope[T <% Ordered[T]](n:T, l:List[T]): Option[T] = {
    def search(l: List[T], r: List[T]): Option[T] =
      if (l == r) None
      else test(l, r, middle(l, r))

    def test(l: List[T], r: List[T], m: List[T]): Option[T] =
      if (n < m.head) search(l, m)
      else if (n > m.head) search(m.tail, r)
      else Some(m.head)

    def middle(l: List[T], r: List[T]): List[T] = {
      @tailrec
      def race(t: List[T], h: List[T]): List[T] =
        if (h != r && h.tail != r)
          race(t.tail, h.tail.tail)
        else t

      race(l, l.tail)
    }

    search(l, Nil)
  }
  def binary[T](n:T, l:List[T]) = binaryScope(n, l)

  /**
    * Linear search algorithm
    * @param n <Int>
    * @param l <List>
    * @return sought value
    * Complexity O(n)
    */
  private[this] def linearScope[T](n:T, l:List[T]): Option[T] = {
    @tailrec
    def iter(list: List[T]): Option[T] =
      if (list.isEmpty) None
      else if (list.head == n) Some(list.head)
      else iter(list.tail)

    iter(l)
  }
  def linear[T](n:T, l:List[T]) = linearScope(n, l)
}