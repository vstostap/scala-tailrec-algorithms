/**
  * @author vstostap
  * @see https://github.com/vstostap/scala-tailrec-algorithms
  */

import scala.annotation.tailrec


package object search {

  /**
    * Binary search algorithm
    * @param n <T>
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
      def iter(t: List[T], h: List[T]): List[T] =
        if (h != r && h.tail != r)
          iter(t.tail, h.tail.tail)
        else t

      iter(l, l.tail)
    }

    search(l, Nil)
  }
  def binary[T <% Ordered[T]](n:T, l:List[T]) = binaryScope(n, l)

  /**
    * Linear search algorithm
    * @param n <T>
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
  def linear[T <% Ordered[T]](n:T, l:List[T]) = linearScope(n, l)


  /**
    * Selection search algorithm
    * @param n <Int>
    * @param l <List>
    * @return sought value
    * Complexity O(n)
    */
  private[this] def selectionScope[T <% Ordered[T]](n: Int, l: List[T]): Option[T] = {
  def search(t: (List[T], T, List[T]), m: Int): Option[T] = t match {
      case (Nil, p, Nil) => Some(p)
      case (s, p, g) => select(s, p, g, s.length, m)
    }

    def select(l: List[T], p: T, g: List[T], q: Int, m: Int): Option[T] =
      if (m < q) partitionAndSearch(l, m)
      else if (m > q) partitionAndSearch(g, m - q - 1)
      else Some(p)

    /**
      * The same as in quicksort.
      */
    def partition(as: List[T]): (List[T], T, List[T]) = {
      @tailrec
      def iter(p: T, as: List[T], l: List[T], g: List[T]): (List[T], T, List[T]) =
        as match {
          case h :: t => if (h < p) iter(p, t, h :: l, g) else iter(p, t, l, h :: g)
          case Nil => (l, p, g)
        }

      iter(as.head, as.tail, Nil, Nil)
    }

    def partitionAndSearch(as: List[T], m: Int): Option[T] =
      if (as.isEmpty) None
      else search(partition(as), m)

    partitionAndSearch(l, n)
  }
  def selection[T <% Ordered[T]](n:Int, l:List[T]) = selectionScope(n, l)
}