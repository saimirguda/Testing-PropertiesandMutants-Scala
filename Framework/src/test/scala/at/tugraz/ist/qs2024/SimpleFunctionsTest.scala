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


  // ------------------------------------------------------------------------------------------------------------
  // maximum
  // ------------------------------------------------------------------------------------------------------------
  // TODO: max() properties

  // ------------------------------------------------------------------------------------------------------------
  // minimal index
  // ------------------------------------------------------------------------------------------------------------
  // TODO: minIndex() properties

  // ------------------------------------------------------------------------------------------------------------
  // symmetric difference
  // ------------------------------------------------------------------------------------------------------------
  // TODO: symmetricDifference() properties

  // ------------------------------------------------------------------------------------------------------------
  // intersection
  // ------------------------------------------------------------------------------------------------------------
  // TODO: intersection() properties

  // ------------------------------------------------------------------------------------------------------------
  // Smallest missing positive integer
  // ------------------------------------------------------------------------------------------------------------
  // TODO: smallestMissingPositiveInteger() properties

  // ------------------------------------------------------------------------------------------------------------
  // isPalindrome
  // ------------------------------------------------------------------------------------------------------------
  // TODO: isPalindrome() properties

  // ------------------------------------------------------------------------------------------------------------
  // Greatest Common Divisor (GCD)
  // ------------------------------------------------------------------------------------------------------------
  // TODO: gcd() properties
  
}
