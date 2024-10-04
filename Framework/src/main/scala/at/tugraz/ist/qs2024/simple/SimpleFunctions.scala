package at.tugraz.ist.qs2024.simple

import scala.annotation.tailrec

object SimpleFunctions {

  // -------------------------------------------------------------------------------------------------------------------
  // Insertion sort

  /**
   * Insertion sort
   *
   * @param xs List to sort
   * @return sorted list
   */
  def insertionSort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case a :: as => insert(a, insertionSort(as))
  }


  /**
   * Insertion sort helper method.
   * Inserts a new element x into already sorted list xs
   *
   * @param x  new element to insert
   * @param xs already sorted list
   * @return
   */
  private def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case a :: as =>
      if (a >= x) x :: xs
      else a :: insert(x, as)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Maximum

  /**
   * Get largest integer/maximum of list
   *
   * @param xs a non-empty list
   * @return largest integer
   */
  def max(xs: List[Int]): Int = xs match {
    case a :: as => max(a, as)
  }

  /**
   * Tail recursive helper function for maximum.
   * Gives maximum of integer x and list xs
   *
   * @param x  a integer
   * @param xs a list
   * @return maximum of x and xs
   */
  @scala.annotation.tailrec
  private def max(x: Int, xs: List[Int]): Int = xs match {
    case Nil => x
    case a :: as =>
      if (x >= a) max(x, as)
      else max(a, as)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Min Index

  /**
   * Get the index of the smallest element of a list
   *
   * @param xs a non-empty list
   * @return the index of the smallest element
   */
  def minIndex(xs: List[Int]): Int = xs match {
    case a :: as => minIndex(as, 0, a, 1)
  }

  /**
   * Tail recursive helper function for minIndex.
   *
   * @param xs           a list
   * @param minimalIndex the current index of the minimal value
   * @param minimalValue the current minimal value
   * @param currentIndex the list index of the current element
   * @return the index of the smallest element
   */
  @scala.annotation.tailrec
  private def minIndex(xs: List[Int], minimalIndex: Int, minimalValue: Int, currentIndex: Int): Int = xs match {
    case Nil => minimalIndex
    case a :: as =>
      if (a < minimalValue) minIndex(as, currentIndex, a, currentIndex + 1)
      else minIndex(as, minimalIndex, minimalValue, currentIndex + 1)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Symmetric Difference

  /**
   * Get a set of elements that are in one of the two sets, but not in both
   *
   * @param xs first set (as List)
   * @param ys second set (as List)
   * @return the symmetric difference of xs and ys (as List)
   */
  def symmetricDifference(xs: List[Int], ys: List[Int]): List[Int] =
    (symmetricDifference(xs, ys, Nil) ++ symmetricDifference(ys, xs, Nil)).distinct


  /**
   * Tail recursive helper function for symmetric difference.
   * Adds elements of addList to the result, that are not in checkList
   *
   * @param addList   list of elements to add to result
   * @param checkList list of elements to compare
   * @param result    intermediate result
   * @return addList without checklist
   */
  @scala.annotation.tailrec
  private def symmetricDifference(addList: List[Int], checkList: List[Int], result: List[Int]): List[Int] =
    addList match {
      case Nil => result.reverse
      case a :: as =>
        if (checkList.contains(a)) symmetricDifference(as, checkList, result)
        else symmetricDifference(as, checkList, a :: result)
    }

  // -------------------------------------------------------------------------------------------------------------------
  // Intersection

  /**
   * Get a set of elements that are in both sets
   *
   * @param xs first set (as List)
   * @param ys second set (as List)
   * @return intersection of xs and ys
   */
  def intersection(xs: List[Int], ys: List[Int]): List[Int] =
    intersection(xs, ys, Nil).distinct

  /**
   * Tail recursive helper function for intersection.
   * Adds elements of addList to the result, that are in checkList
   *
   * @param addList   list of elements to add to result
   * @param checkList list of elements to compare
   * @param result    intermediate result
   * @return addList intersected with checklist
   */
  @scala.annotation.tailrec
  private def intersection(addList: List[Int], checkList: List[Int], result: List[Int]): List[Int] =
    addList match {
      case Nil => result
      case a :: as =>
        if (checkList.contains(a)) intersection(as, checkList, a :: result)
        else intersection(as, checkList, result)
    }


  // -------------------------------------------------------------------------------------------------------------------
  // Smallest missing positive integer

  /**
   * Gives the smallest integer > 0 that is not in the given list.
   *
   * @param xs the list to check
   * @return smallest missing positive integer
   */
  def smallestMissingPositiveInteger(xs: List[Int]): Int = {
    val arrayCopy: Array[Int] = xs.toArray

    var j: Int = 0
    for (i <- arrayCopy.indices) {
      if (arrayCopy(i) <= 0) {
        val temp = arrayCopy(i)
        arrayCopy(i) = arrayCopy(j)
        arrayCopy(j) = temp
        j += 1
      }
    }

    val array2 = arrayCopy.slice(j, arrayCopy.length)

    for (i <- array2.indices) {
      j = math.abs(array2(i)) - 1
      if (j < array2.length && array2(j) > 0) {
        array2(j) = -array2(j)
      }
    }

    for (i <- array2.indices) {
      if (array2(i) > 0) {
        return i + 1
      }
    }

    array2.length + 1
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Check if a String is a Palindrome

  /**
   * Returns True if the passed String is indeed a Palindrome, or False otherwise
   *
   * @param s the String to check
   * @return Boolean
   */
    def isPalindrome(s: String): Boolean = {
      s == s.reverse
    }

  // -------------------------------------------------------------------------------------------------------------------
  //  Calculates the GCD of two Integers (tail-recursive)

  /**
   * Returns the GCD as an Integer of the two passed Integers using tail-recursion
   *
   * @param a the first Integer
   * @param b the second Integer
   * @return Integer: The GCD of a and b -> Check here again how Euclid works maybe and also what @tailrec checks!
   */
    @tailrec
    def gcd(a: Int, b: Int): Int = {
      if (b == 0) math.abs(a)
      else gcd(b, a % b)
    }

  // -------------------------------------------------------------------------------------------------------------------
  //  Calculates the Mean of a given List of Integers

  /**
   * Returns the arithmetic mean of a given List of Integers
   *
   * @param xs the List of Integers
   * @return The arithmetic mean of the List of Integers.
   */
  def mean(xs: List[Int]): Double = {
    if (xs.isEmpty) 0.0
    else xs.sum.toDouble / xs.length
  }

  // -------------------------------------------------------------------------------------------------------------------
  //  Calculates the Median of a given List of Integers

  /**
   * Returns the median of a given List of Integers
   *
   * @param xs the List of Integers
   * @return The median of the List of Integers. Be aware of the edge case when xs has even length!
   */
  def median(xs: List[Int]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      val sorted = xs.sorted
      val mid = sorted.length / 2
      if (sorted.length % 2 == 0) Some((sorted(mid - 1) + sorted(mid)).toDouble / 2)
      else Some(sorted(mid))
    }
  }

}
