package at.tugraz.ist.qs2024

import at.tugraz.ist.qs2024.simple.SimpleFunctions._
//import at.tugraz.ist.qs2024.simple.SimpleFunctionsMutant1._
//import at.tugraz.ist.qs2024.simple.SimpleFunctionsMutant2._
//import at.tugraz.ist.qs2024.simple.SimpleFunctionsMutant3._
//import at.tugraz.ist.qs2024.simple.SimpleFunctionsMutant4._
import at.tugraz.ist.qs2024.simple.SimpleJavaFunctions
import org.junit.runner.RunWith
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.annotation.tailrec

// Consult the following scalacheck documentation
// https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#concepts
// https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#generators

@RunWith(classOf[ScalaCheckJUnitPropertiesRunner])
class SimpleFunctionsTest extends Properties("SimpleFunctionsTest") {

  // Gen is some sort of function from scala check,
  // it is responsible to provide you random generated test data
  private val nonEmptyIntListGen: Gen[List[Int]] = Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])
  private val randomIntGen: Gen[Int] = Arbitrary.arbitrary[Int]
  private val randomStringGen: Gen[String] = Arbitrary.arbitrary[String]

  // TODO: ADD a Generator for isPalindrome() property
  //Generator: creates a palindrome by appending the reverse of a generated string (word) to itself.
  val generatePalindrome: Gen[String] = for {
    word <- Gen.alphaStr
    reversedWord = word.reverse
  } yield word + reversedWord


  // ------------------------------------------------------------------------------------------------------------
  // insertionSort Java style
  // ------------------------------------------------------------------------------------------------------------
  property("insertionSort Java: ordered") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val sorted = SimpleJavaFunctions.insertionSort(xs.toArray)
    var correctFlag = true;
    if (xs.nonEmpty) {
      for (i <- 0 until sorted.length - 1) {
        if (sorted(i) > sorted(i + 1))
          correctFlag = false;
      }
      correctFlag // would be the return val
    }
    else
      false // returns false if xs is empty
  }

  // ------------------------------------------------------------------------------------------------------------
  // insertionSort the beautiful scala way
  // ------------------------------------------------------------------------------------------------------------
  property("insertionSort: ordered") = forAll(nonEmptyIntListGen) { (xs: List[Int]) =>
    val sorted = insertionSort(xs)
    xs.nonEmpty ==> xs.indices.tail.forall((i: Int) => sorted(i - 1) <= sorted(i))
  }

  property("insertionSort: permutation") = forAll { (xs: List[Int]) =>
    val sorted = insertionSort(xs)

    def count(a: Int, as: List[Int]) = as.count(_ == a)

    xs.forall((x: Int) => count(x, xs) == count(x, sorted))
  }

  // TODO: Think of more properties for insertionSort

  //Property: check that negative numbers can be sorted correctly
  property("insertionSort: negative numbers") = forAll { (xs: List[Int]) =>
    val sorted = insertionSort(xs)
    //sorted.zip(sorted.tail) => pairing each element in the sorted list with its next element.
    //check if every element is less than or equal to the next element
    xs.nonEmpty ==> sorted.zip(sorted.tail).forall { case (a, b) => a <= b }
  }

  //Property: once a list has been sorted, sorting it again should not change the order
  property("insertionSort: sorting already sorted list") = forAll { (xs: List[Int]) =>
    val sortedOnce = insertionSort(xs)
    val sortedTwice = insertionSort(sortedOnce)
    sortedOnce == sortedTwice
  }

  //Property: sorting a reversed list should result in a list that is sorted in ascending order,
  // just like sorting the original list with scala built-in .sorted method
  property("insertionSort: reverse ordered lists") = forAll { (xs: List[Int]) =>
    val reverseSorted = insertionSort(xs.reverse)
    reverseSorted == xs.sorted
  }

  //Property: sorting empty list should return empty list
  property("insertionSort: empty list") = forAll { (xs: List[Int]) =>
    insertionSort(List.empty[Int]) == List.empty[Int]
  }

  //Property: calling sort on a list with 1 element should result a list with a single element
  property("insertionSort: single element list") = forAll { (xs: List[Int]) =>
    forAll { (x: Int) =>
      insertionSort(List(x)) == List(x)
    }
  }

  //Property: compare scala built-in .sorted with our implementation, result should be the same
  property("insertionSort: comparison with scala built-in") = forAll { (xs: List[Int]) =>
    val builtInSorted = xs.sorted
    val localSorted = insertionSort(xs)
    builtInSorted == localSorted
  }


  // ------------------------------------------------------------------------------------------------------------
  // maximum
  // ------------------------------------------------------------------------------------------------------------
  // TODO: max() properties

  //Property: ensures that the value returned by the max function actually exists in the input list
  property("max: return value is an element of the list") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val maxValue = max(xs)
      xs.contains(maxValue)
    }
  }

  //Property: no element in the list is greater than the value returned by the max function
  property("max: is greater than or equal to all elements") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val maxValue = max(xs)
      xs.forall(_ <= maxValue)
    }
  }

  //Property: maximum value of a list remains the same even after shuffling the list
  property("max: shuffling the list") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val maxValue = max(xs)
      val shuffledList = scala.util.Random.shuffle(xs)
      maxValue == max(shuffledList)
    }
  }

  //Property: correctly identify max of single element list
  property("max: single element list") = forAll { (x: Int) =>
    max(List(x)) == x
  }

  //Property: correctly identifies the maximum value in lists composed entirely of negative numbers
  property("max: negative numbers") = forAll { (xs: List[Int]) =>
    (!xs.isEmpty && xs.forall(_ < 0)) ==> {
      val maxValue = max(xs)
      xs.forall(_ <= maxValue)
    }
  }

  //Property: have multiple occurrences of the same maximum value, remove all but one and check if the maximum remains
  // the same after removing all other instances and calling max on the list again
  property("max: find maximum after removing duplicates of the maximum") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val maxValue = max(xs)
      //remove all occurrences of the maximum, afterwards append maximum so that the list contains 1 instance of it
      val cleanedList = xs.filter(_ != maxValue) :+ maxValue
      max(cleanedList) == maxValue
    }
  }

  // ------------------------------------------------------------------------------------------------------------
  // minimal index
  // ------------------------------------------------------------------------------------------------------------
  // TODO: minIndex() properties

  //Property: value at the index returned by minIndex is indeed the minimum value in the list.
  property("minIndex: is the minimum value") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val index = minIndex(xs)
      val minValue = xs(index)
      xs.forall(_ >= minValue)
    }
  }

  //Property: checks for a valid index which would be between 0 and the length of the list
  property("minIndex: valid index check") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val index = minIndex(xs)
      (index >= 0) && (index < xs.length)
    }
  }

  //Property: removing elements that are larger than the minimum element,
  // the index of the minimum value should remain the same
  property("minIndex: removing larger elements from list") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val minIdx = minIndex(xs)
      val minValue = xs(minIdx)
      val filteredList = xs.filter(_ <= minValue)
      minIndex(filteredList) == 0
    }
  }

  //Property: compare result of minimum value returned by scala built-in min method
  property("minIndex: comparison with scala built-in") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val index = minIndex(xs)
      xs(index) == xs.min
    }
  }

  //Property: minimum element is duplicated, the index of the first occurrence should remain unchanged
  property("minIndex: appending duplicates of minimum") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      val minVal = xs.min
      //append two duplicates of the minimum value
      val newXs = xs ++ List.fill(2)(minVal)
      minIndex(newXs) == minIndex(xs)
    }
  }


  // ------------------------------------------------------------------------------------------------------------
  // symmetric difference
  // ------------------------------------------------------------------------------------------------------------
  // TODO: symmetricDifference() properties

  //Property: symmetric difference removes common elements from both lists
  property("symmetricDifference: the are no common elements") = forAll { (xs: List[Int], ys: List[Int]) =>
    val result = symmetricDifference(xs, ys)
    //intersection to find common elements between the two lists
    val identElements = xs.intersect(ys)
    //resulting list should not contain any identical elements
    result.intersect(identElements).isEmpty
  }

  //Property: when two identical lists are passed, their symmetric difference should be empty
  // because all elements are common and thus excluded.
  property("symmetricDifference: call on self, result is empty") = forAll { (xs: List[Int]) =>
    symmetricDifference(xs, xs).isEmpty
  }

  //Property: the symmetric difference between any list and an empty list, the result should be
  // the original list(duplicates excluded)
  property("symmetricDifference: with empty list returns original unique elements") = forAll { (xs: List[Int]) =>
    val uniqueList = xs.distinct
    (symmetricDifference(xs, List.empty[Int]).sorted == uniqueList.sorted) &&
      (symmetricDifference(List.empty[Int], xs).sorted == uniqueList.sorted)
  }

  //Property: if two lists have no common elements, the result of symmetric difference
  // should be the same as the union of both lists
  property("symmetricDifference: no common elements") = forAll { (xs: List[Int], ys: List[Int]) =>
    val commonIntegers = xs.intersect(ys).distinct //determine common elements(excluding duplicates)
    val symmetricDifferenceList = symmetricDifference(xs, ys)
    //confirm there are no common elements with .isEmpty and
    // compare if symmetric difference is equal to the union of the original lists
    commonIntegers.isEmpty ==> (symmetricDifferenceList.sorted == (xs ++ ys).sorted.distinct)
  }


  // ------------------------------------------------------------------------------------------------------------
  // intersection
  // ------------------------------------------------------------------------------------------------------------
  // TODO: intersection() properties

  //Property: the intersection between two lists should result in the same both ways
  property("intersection: commutative intersection") = forAll { (xs: List[Int], ys: List[Int]) =>
    intersection(xs, ys).sorted == intersection(ys, xs).sorted
  }

  //Property: compare result of our implementation of intersection with scala built-in intersection
  property("intersection: comparison with scala built-in") = forAll { (xs: List[Int], ys: List[Int]) =>
    val intersectionResult = intersection(xs, ys)
    val scalaIntersection = xs.intersect(ys)
    intersectionResult.sorted.distinct == scalaIntersection.sorted.distinct
  }

  //Property: result of intersection should contain element exclusively present in both input lists
  property("intersection: only common elements") = forAll { (xs: List[Int], ys: List[Int]) =>
    val result = intersection(xs, ys)
    result.forall(n => xs.contains(n) && ys.contains(n))
  }

  //Property: intersection of a list with itself should return the unique elements of that list
  property("intersection: self intersect") = forAll { (xs: List[Int]) =>
    val result = intersection(xs, xs)
    result.sorted == xs.distinct.sorted
  }

  //Property: calling intersect on an empty list results in an empty list
  property("intersection: with empty list") = forAll { (xs: List[Int]) =>
    (intersection(xs, List.empty[Int]).isEmpty) &&
      (intersection(List.empty[Int], xs).isEmpty)
  }


  // ------------------------------------------------------------------------------------------------------------
  // Smallest missing positive integer
  // ------------------------------------------------------------------------------------------------------------
  // TODO: smallestMissingPositiveInteger() properties

  //Property: integer returned by the function is not present in the input list
  property("smallestMissingPositiveInteger: resulting integer is not part of input list") = forAll { (xs: List[Int]) =>
    val result = smallestMissingPositiveInteger(xs)
    !xs.contains(result)
  }

  //Property: function should always return a positive integer
  property("smallestMissingPositiveInteger: always positive") = forAll { (xs: List[Int]) =>
    smallestMissingPositiveInteger(xs) > 0
  }

  //Property: ensure that adding the result of smallestMissingPositiveInteger(xs) to the input list xs always
  // increases the minimum missing positive integer
  property("smallestMissingPositiveInteger: adding result to input increases minimum") = forAll { (xs: List[Int]) =>
    val resultingInteger = smallestMissingPositiveInteger(xs)
    val newList = resultingInteger :: xs
    val newResult = smallestMissingPositiveInteger(newList)
    newResult > resultingInteger
  }

  //Property: non-positive integers should not affect the result therefor removing them from the list should not
  // change the smallest missing positive integer
  property("smallestMissingPositiveInteger: remove negative from list") = forAll { (xs: List[Int]) =>
    val result = smallestMissingPositiveInteger(xs)
    val negativeRemoved = smallestMissingPositiveInteger(xs.filter(_ > 0))
    result == negativeRemoved
  }

  //Property: adding duplicates should not affect result
  property("smallestMissingPositiveInteger: adding duplicates") = forAll { (xs: List[Int]) =>
    val result = smallestMissingPositiveInteger(xs)
    val duplicatedResult = xs ++ xs
    val resultDuplicates = smallestMissingPositiveInteger(duplicatedResult)
    result == resultDuplicates
  }


  // ------------------------------------------------------------------------------------------------------------
  // isPalindrome
  // ------------------------------------------------------------------------------------------------------------
  // TODO: isPalindrome() properties

  //Property: identifies palindrome correctly
  property("isPalindrome: recognize palindrome") = forAll(generatePalindrome) { (palindrome: String) =>
    isPalindrome(palindrome)
  }

  //Property: test if non-palindromes are identified
  property("isPalindrome: recognize non-palindrome") = forAll(generatePalindrome) { palindrome: String =>
    val manipulatedNonPalindrome = if (palindrome.isEmpty) "xy" else palindrome + "x"
    !isPalindrome(manipulatedNonPalindrome)
  }

  //Property: test case insensitivity for generated palindrome
  property("isPalindrome: palindrome case insensitivity") = forAll(generatePalindrome) { palindrome =>
    isPalindrome(palindrome.toLowerCase) && isPalindrome(palindrome.toUpperCase)
  }

  //Property: any string and its reverse should yield the same result when passed to isPalindrome
  property("isPalindrome: symmetric property") = forAll { (anyStr: String) =>
    isPalindrome(anyStr) == isPalindrome(anyStr.reverse)
  }

  //Property: case insensitivity for any string yields the same result when passed to isPalindrome
  property("isPalindrome: case insensitivity") = forAll { (anyStr: String) =>
    isPalindrome(anyStr.toLowerCase) == isPalindrome(anyStr.toUpperCase)
  }

  //Property: empty string and single character are palindromes
  property("isPalindrome: empty string and single character are palindromes") = forAll(Gen.oneOf("", "x", "1")) { (s: String) =>
    isPalindrome(s)
  }

  //Property: correctly identifies palindromes of both even and odd lengths
  property("isPalindrome: even and odd lengths") = forAll { (s: String) =>
    val evenPalindrome = s + s.reverse
    val oddPalindrome = s + "x" + s.reverse
    isPalindrome(evenPalindrome) && isPalindrome(oddPalindrome)
  }

  //Property: concatenating two palindromes does not always result in a palindrome
  property("isPalindrome: concatenation of palindromes not a palindrome") = forAll(generatePalindrome, generatePalindrome) { (p1, p2) =>
    (!p1.isEmpty && !p2.isEmpty && p1 != p2) ==> !isPalindrome(p1 + p2)
  }


  // ------------------------------------------------------------------------------------------------------------
  // Greatest Common Divisor (GCD)
  // ------------------------------------------------------------------------------------------------------------
  // TODO: gcd() properties

  //Property: gcd of any number and zero is the absolute value of the non-zero number
  property("gcd: zero") = forAll { (n: Int) =>
    (gcd(n, 0) == math.abs(n)) &&
      (gcd(0, n) == math.abs(n))
  }

  //Property: gcd of any number with itself is the number itself
  property("gcd: identical numbers") = forAll { (n: Int) =>
    gcd(n, n) == math.abs(n)
  }

  //Property: checks that the gcd indeed divides both of the numbers without a remainder
  property("gcd: result divides both numbers") = forAll { (x: Int, y: Int) =>
    val result = gcd(x, y)
    (result == 0 || ( (x % result == 0) && (y % result == 0) ))
  }

  //Property: gcd should be the same regardless of the order of the operands
  property("gcd: commutative") = forAll { (x: Int, y: Int) =>
    gcd(x, y) == gcd(y, x)
  }

  //Property: the gcd of three numbers should be the same no matter how the pairs are grouped
  property("gcd: associative") = forAll { (x: Int, y: Int, z: Int) =>
    gcd(x, gcd(y, z)) == gcd(gcd(x, y), z)
  }

}
