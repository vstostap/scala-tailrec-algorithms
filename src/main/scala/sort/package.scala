/**
  * @author vstostap
  * @see https://github.com/vstostap/scala-tailrec-algorithms
  */

import scala.annotation.tailrec

package object sort {

  /**
    * Bubble sort
    * @param list <List>
    * @return sorted list
    * Complexity O(n*n)
    */
  private[this] def bubblesort[T <% Ordered[T]](list: List[T]): List[T] = {
    def sort(as: List[T], bs: List[T]): List[T] =
      if (as.isEmpty) bs
      else bubble(as, Nil, bs)
    @tailrec
    def bubble(as: List[T], zs: List[T], bs: List[T]): List[T] = as match {
      case h1 :: h2 :: t =>
        if (h1 > h2) bubble(h1 :: t, h2 :: zs, bs)
        else bubble(h2 :: t, h1 :: zs, bs)
      case h1 :: Nil => sort(zs, h1 :: bs)
    }

    sort(list, Nil)
  }
  def bubble[T <% Ordered[T]](list: List[T]) = bubblesort(list)

  /**
    * Insertion sort
    * @param list <List>
    * @return sorted list
    * Complexity O(n*n)
    */
  private[this] def insertionsort[T <% Ordered[T]](list: List[T]): List[T] = {
    @tailrec
    def sort(as: List[T], bs: List[T]): List[T] = as match {
      case h :: t => sort(t, insert(h, bs))
      case Nil => bs
    }

    def insert(a: T, as: List[T]): List[T] = as match {
      case h :: t if (a > h) => h :: insert(a, t)
      case _ => a :: as
    }

    sort(list, Nil)
  }
  def insertion[T <% Ordered[T]](list: List[T]) = insertionsort(list)

  /**
    * Selection sort
    * @param list <List>
    * @return sorted list
    * Complexity O(n*n)
    */
  private[this] def selectionsort[T <% Ordered[T]](list: List[T]): List[T] = {
    def sort(as: List[T], bs: List[T]): List[T] = as match {
      case h :: t => select(h, t, Nil, bs)
      case Nil => bs
    }

    @tailrec
    def select(m: T, as: List[T], zs: List[T], bs: List[T]): List[T] =
      as match {
        case h :: t =>
          if (m > h) select(m, t, h :: zs, bs)
          else select(h, t, m :: zs, bs)
        case Nil => sort(zs, m :: bs)
      }

    sort(list, Nil)
  }
  def selection[T <% Ordered[T]](list: List[T]) = selectionsort(list)

  /**
    * Merge sort
    * @param list <List>
    * @return sorted list
    * Complexity O(n*log n)
    */
  private[this] def mergesort[T <% Ordered[T]](list: List[T]): List[T] = {
    def sort(p: (List[T], List[T])): List[T] = p match {
      case (Nil, Nil) => Nil
      case (ha :: Nil, Nil) => ha :: Nil
      case (Nil, hb :: Nil) => hb :: Nil
      case (as, bs) => merge(iter(as), iter(bs))
    }

    def halfify(as: List[T]): (List[T], List[T]) = {
      @tailrec
      def loop(bs: List[T], fs: List[T], ss: List[T]): (List[T], List[T]) = bs match {
        case f :: s :: r => loop(r, f :: fs, s :: ss)
        case f :: Nil => (f :: fs, ss)
        case Nil => (fs, ss)
      }

      loop(as, Nil, Nil)
    }

    def merge(as: List[T], bs: List[T]): List[T] = {
      @tailrec
      def loop(cs: List[T], ds: List[T], r: List[T]): List[T] = (cs, ds) match {
        case (ha :: ta, hb :: tb) =>
          if (ha < hb) loop(ta, ds, ha :: r)
          else loop(cs, tb, hb :: r)
        case (ha :: ta, Nil) => loop(ta, Nil, ha :: r)
        case (Nil, hb :: tb) => loop(Nil, tb, hb :: r)
        case (Nil, Nil) => r
      }

      loop(as, bs, Nil).reverse
    }

    def iter(as: List[T]) = sort(halfify(as))

    iter(list)
  }
  def merge[T <% Ordered[T]](list: List[T]) = mergesort(list)

  /**
    * Quick sort
    * @param list <List>
    * @return sorted list
    * Complexity O(n*log n)
    */
  private[this] def quicksort[T <% Ordered[T]](list: List[T]): List[T] = {
    def sort(t: (List[T], T, List[T])): List[T] = t match {
      case (Nil, p, Nil) => List(p)
      case (l, p, g) =>  partitionAndSort(l) ::: (p :: partitionAndSort(g))
    }

    def partition(as: List[T]): (List[T], T, List[T]) = {
      @tailrec
      def iter(p: T, as: List[T], l: List[T], g: List[T]): (List[T], T, List[T]) =
        as match {
          case h :: t => if (h < p) iter(p, t, h :: l, g) else iter(p, t, l, h :: g)
          case Nil => (l, p, g)
        }

      iter(as.head, as.tail, Nil, Nil)
    }

    def partitionAndSort(as: List[T]): List[T] =
      if (as.isEmpty) Nil
      else sort(partition(as))

    partitionAndSort(list)
  }
  def quick[T <% Ordered[T]](list: List[T]) = quicksort(list)
}