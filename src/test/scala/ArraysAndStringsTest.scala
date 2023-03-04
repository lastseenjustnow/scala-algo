import ArraysAndStrings._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.util.Random

class ArraysAndStringsTest extends FunSuite {

  test("Two Sum") {
    val conditions: Array[(Array[Int], Int, Array[Int])] =
      Array(
        (Array(2, 7, 11, 15), 9, Array(0, 1)),
        (Array(3, 2, 4), 6, Array(1, 2)),
        (Array(3, 3), 6, Array(0, 1)),
      )

    for (cond <- conditions) {
      assert(twoSum(cond._1, cond._2) sameElements cond._3)
      assert(twoSumReturn(cond._1, cond._2) sameElements cond._3)
    }

    val testcase = (Array(2, 5, 3, 4, 6, 11), 7, Array((2, 5), (3, 4)))
    assert(twoSumBuffer(testcase._1, testcase._2) sameElements testcase._3)

  }

  test("Max Number of K-Sum Pairs") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(1, 2, 3, 4), 5, 2),
        (Array(3, 1, 3, 4, 3), 6, 1),
        (Array(1), 0, 0),
        (Array(1, 2), 3, 1),
        (Array(2, 5, 4, 4, 1, 3, 4, 4, 1, 4, 4, 1, 2, 1, 2, 2, 3, 2, 4, 2), 3, 4)
      )

    for (cond <- conditions) {
      assert(maxOperations(cond._1, cond._2) == cond._3)
    }
  }

  test("String to Integer (atoi)") {
    val conditions: Array[(String, Int)] =
      Array(
        ("42", 42),
        ("   -42", -42),
        ("4193 with words", 4193),
        ("", 0),
        ("4193 w26", 4193),
        (".4193 w26", 0),
        (". 4193 w26", 0),
        ("words and 987", 0),
        ("-432+", -432),
        ("-91283472332", Int.MinValue),
        ("91283472332", Int.MaxValue),
        ("+-12", 0),
        ("-0032", -32),
        ("20000000000000000000", Int.MaxValue)
      )

    for (cond <- conditions) {
      assert(myAtoi(cond._1) == cond._2)
    }
  }

  test("Insert Interval") {
    val conditions: Array[(Array[Array[Int]], Array[Int], Array[Array[Int]])] =
      Array(
        (Array(Array(1, 3), Array(6, 9)), Array(2, 5), Array(Array(1, 5), Array(6, 9))),
        (Array(Array(1, 2), Array(3, 5), Array(6, 7), Array(8, 10), Array(12, 16)), Array(4, 8), Array(Array(1, 2), Array(3, 10), Array(12, 16))),
        (Array(Array(1, 3), Array(4, 6), Array(7, 9)), Array(2, 5), Array(Array(1, 6), Array(7, 9))),
        (Array(Array(1, 3), Array(5, 5), Array(4, 6), Array(7, 9)), Array(2, 5), Array(Array(1, 6), Array(7, 9))),
        (Array(), Array(2, 5), Array(Array(2, 5))),
        (Array(Array(1, 2)), Array(2, 5), Array(Array(1, 5))),
        (Array(Array(1, 2)), Array(3, 5), Array(Array(1, 2), Array(3, 5))),
        (Array(Array(1, 3), Array(6, 9)), Array(4, 5), Array(Array(1, 3), Array(4, 5), Array(6, 9))),
      )

    for (cond <- conditions) {
      insert(cond._1, cond._2) should equal(cond._3)
    }
  }


  test("K-diff Pairs in an Array") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 1, 4, 1, 5), 2, 2),
        (Array(1, 2, 3, 4, 5), 1, 4),
        (Array(1, 3, 1, 5, 4), 0, 1),
        (Array(1), 1, 0),
        (Array(1, 3, 1, 0, 5, 4, -1, 2), 2, 5),
        (Array(1, 2, 4, 4, 3, 3, 0, 9, 2, 3), 3, 2)
      )

    for (cond <- conditions) {
      findPairs(cond._1, cond._2) should equal(cond._3)
    }
  }

  test("Minimum Space Wasted From Packaging") {
    val conditions: Array[(Array[Int], Array[Array[Int]], Int)] =
      Array(
        (Array(1, 3, 4, 6, 8, 9, 10, 12), Array(Array(5, 8, 12)), 14),
        (Array(1, 3, 4, 6, 6, 8, 9, 10, 12), Array(Array(5, 8, 12)), 16),
        (Array(1, 3, 4, 6, 8, 6, 9, 8, 10, 12), Array(Array(5, 8, 13)), 19),
        (Array(1, 3, 4, 6, 8, 6, 9, 10, 12), Array(Array(5, 8, 13)), 19),
        (Array(1, 3, 4, 6, 8, 9, 10, 12), Array(Array(5, 8, 13), Array(12, 8, 8, 5), Array(5, 8, 8, 12), Array(5, 8, 12)), 14),
        (Array(2, 3, 5), Array(Array(4, 8), Array(2, 8)), 6),
        (Array(2, 3, 5), Array(Array(1, 4), Array(2, 3), Array(3, 4)), -1),
        (Array(3, 5, 8, 10, 11, 12), Array(Array(12), Array(11, 9), Array(10, 5, 14)), 9),
        (Array(1), Array(Array(12), Array(11, 9), Array(10, 5, 14)), 4),
        (Array(1), Array(Array(2), Array(3)), 1),
        (Array(7, 6, 5, 3, 4), Array(Array(2, 7), Array(6), Array(10, 5)), 10)
      )

    for (cond <- conditions) {
      minWastedSpaceBruteForce(cond._1, cond._2) should equal(cond._3)
      minWastedSpace(cond._1, cond._2) should equal(cond._3)
    }

    val testcase = ((for (i <- 1 to 100000) yield i).toArray, (for (i <- 50000 until 100000) yield Array(i, 100000)).toArray, 499949986)
    minWastedSpace(testcase._1, testcase._2) should equal(testcase._3)

  }

  test("Longest Substring Without Repeating Characters") {
    val conditions: Array[(String, Int)] =
      Array(
        ("abcabcbb", 3),
        ("bbbbb", 1),
        ("pwwkew", 3),
        ("", 0),
        ("sadfksksajkajgss", 5)
      )

    for (cond <- conditions) {
      assert(lengthOfLongestSubstring(cond._1) == cond._2)
      assert(lengthOfLongestSubstring2(cond._1) == cond._2)
    }
  }

  test("Container With Most Water") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(1, 8, 6, 2, 5, 4, 8, 3, 7), 49),
        (Array(1, 1), 1),
        (Array(1, 8, 6, 2, 1000, 1000, 8, 3, 7), 1000),
        (Array(1, 8, 6, 2, 1000, 4, 8, 3, 7), 49),
        (Array(1, 8, 6, 2, 1000, 2, 1000, 8, 3, 7, 15, 20, 25, 30, 40, 30), 2000),
        (Array(1, 2, 3, 4), 4),
        (Array(1, 5, 4, 1, 1, 1, 1, 1, 1, 1), 9),
        (Array(1, 5, 4, 1, 1, 1, 1, 1, 2, 1), 14),
        (Array(3, 9, 3, 4, 7, 2, 12, 6), 45),
        ((for (i <- 0 to 1000) yield i).toArray, 250000),
      )

    val source = scala.io.Source.fromResource("containerWithMostWater.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()

    for (cond <- conditions) {
      assert(maxAreaBruteForce(cond._1) == cond._2)
      assert(maxArea(cond._1) == cond._2)
      assert(maxArea(testcase) == 887155335)
      assert(maxAreaRecursive(cond._1) == cond._2)
      assert(maxAreaRecursive(testcase) == 887155335)
    }
  }

  test("Remove Element") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 2, 2, 3), 3, 2),
        (Array(0, 1, 2, 2, 3, 0, 4, 2), 2, 5),
        (Array(), 3, 0),
      )

    for (cond <- conditions) {
      assert(removeElement(cond._1, cond._2) == cond._3)
    }
  }

  test("Remove Duplicates from Sorted Array") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(1, 2, 3, 4), 4),
        (Array(0, 0, 1, 1, 1, 2, 2, 3, 4), 5),
        (Array(1, 1, 2), 2),
        (Array(0, 0, 1, 1, 1, 2, 2, 3, 3, 4), 5)
      )

    for (cond <- conditions) {
      assert(removeDuplicates(cond._1) == cond._2)
    }
  }

  test("Check If N and Its Double Exist") {
    val conditions: Array[(Array[Int], Boolean)] = Array(
      (Array(10, 2, 5, 3), true),
      (Array(7, 1, 14, 11), true),
      (Array(3, 1, 7, 11), false),
      (Array(-2, 0, 10, -19, 4, 6, -8), false),
      (Array(-2, 0, 10, -19, 0, 4, 6, -8), true)
    )

    for (cond <- conditions) {
      assert(checkIfExist(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Valid Mountain Array") {
    val conditions: Array[(Array[Int], Boolean)] = Array(
      (Array(2, 1), false),
      (Array(3, 5, 5), false),
      (Array(0, 3, 2, 1), true),
      (Array(-1, 0, 3, 2, 1), true),
      (Array(-4, -3, -2, -1, 0, 3, 7, 1), true),
      (Array(-4, -3, -3, -1, 0, 3, 7, 1), false),
      (Array(-4, -3, -2, -1, 0, 3, 7, 7), false),
      (Array(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), false)
    )

    for (cond <- conditions) {
      assert(validMountainArray(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }


  test("Integer to Roman & Roman to Integer") {
    val conditions: Array[(Int, String)] =
      Array(
        (3, "III"),
        (58, "LVIII"),
        (1994, "MCMXCIV"),
        (1494, "MCDXCIV"),
        (3999, "MMMCMXCIX"),
      )

    for (cond <- conditions) {
      assert(intToRoman(cond._1) == cond._2)
      assert(romanToInt(cond._2) == cond._1)
    }
  }

  test("Valid Parentheses") {
    val conditions: Array[(String, Boolean)] =
      Array(
        ("()", true),
        ("()[]{}", true),
        ("(]", false),
        ("[{(]})", false),
        ("(", false),
        ("]", false),
        ("(" * Math.pow(10, 4).toInt, false)
      )

    for (cond <- conditions) {
      assert(isValid(cond._1) == cond._2)
    }
  }

  test("Replace Elements with Greatest Element on Right Side") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(17, 18, 5, 4, 6, 1), Array(18, 6, 6, 6, 1, -1)),
        (Array(400), Array(-1))
      )

    for (cond <- conditions) {
      assert(replaceElements(cond._1) sameElements cond._2)
    }
  }

  test("Move Zeroes") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(0, 1, 0, 3, 12), Array(1, 3, 12, 0, 0)),
        (Array(0), Array(0)),
        (Array(1, 2, 3, 4, 5), Array(1, 2, 3, 4, 5)),
        (Array(1, 2, 3, 4, 5, 0), Array(1, 2, 3, 4, 5, 0)),
        (Array(1, 2, 3, 0, 4, 5, 0), Array(1, 2, 3, 4, 5, 0, 0)),
      )

    for (cond <- conditions) {
      moveZeroes(cond._1)
      assert(cond._1 sameElements cond._2)
    }
  }

  test("Sort Array By Parity") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(3, 1, 2, 4), Array(2, 4, 3, 1)),
        (Array(3, 1, 2, 5, 4), Array(2, 4, 3, 5, 1)),
        (Array(0), Array(0))
      )

    for (cond <- conditions) {
      assert(sortArrayByParity(cond._1) sameElements cond._2)
    }
  }

  test("Height Checker") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(1, 1, 4, 2, 1, 3), 3),
        (Array(5, 1, 2, 3, 4), 5),
        (Array(1, 2, 3, 4, 5), 0),
      )

    for (cond <- conditions) {
      assert(heightChecker(cond._1) == cond._2)
    }
  }

  test("Max Consecutive Ones II") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(1, 1, 0, 1, 1, 0), 5),
        (Array(1, 0, 1, 1, 0), 4),
        (Array(1, 0, 1, 1, 0, 1), 4),
        (Array(1, 1), 2),
        (Array(0), 1),
        (Array(1), 1),
        (Array(1, 1, 0, 1), 4),
        ((for (i <- 0 until Math.pow(10, 5).toInt) yield i % 2).toArray, 3)
      )

    for (cond <- conditions) {
      assert(findMaxConsecutiveOnes(cond._1) == cond._2)
    }
  }

  test("Third Maximum Number") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(3, 2, 1), 1),
        (Array(1, 2), 2),
        (Array(2, 2, 3, 1), 1),
        (Array(1, 2, -2147483648), -2147483648)
      )

    for (cond <- conditions) {
      assert(thirdMax(cond._1) == cond._2)
    }

    for (cond <- conditions) {
      assert(thirdMax(cond._1) == cond._2)
    }

    val rands = Seq.fill(Math.pow(10, 4).toInt)(Random.nextInt).toArray
    val randsSorted = rands.sorted.reverse
    assert(thirdMax(rands) == randsSorted(2))
  }

  test("Find All Numbers Disappeared in an Array") {
    val conditions: Array[(Array[Int], List[Int])] =
      Array(
        (Array(4, 3, 2, 7, 8, 2, 3, 1), List(5, 6)),
        (Array(1, 1), List(2)),
        (Array(1), List())
      )

    for (cond <- conditions) {
      assert(findDisappearedNumbers(cond._1) == cond._2)
    }
  }

  test("Squares of a Sorted Array") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(-4, -1, 0, 3, 10), Array(0, 1, 9, 16, 100)),
        (Array(-7, -3, 2, 3, 11), Array(4, 9, 9, 49, 121)),
        (Array(1, 2, 3, 4, 6), Array(1, 4, 9, 16, 36)),
        (Array(0), Array(0)),
        (Array(-1, 0), Array(0, 1)),
      )

    for (cond <- conditions) {
      assert(sortedSquares(cond._1) sameElements cond._2)
    }

  }

  test("Reverse String") {
    val conditions: Array[(Array[Char], Array[Char])] =
      Array(
        (Array('h', 'e', 'l', 'l', 'o'), Array('o', 'l', 'l', 'e', 'h')),
        (Array('H', 'a', 'n', 'n', 'a', 'h'), Array('h', 'a', 'n', 'n', 'a', 'H')),
        (Array('H'), Array('H')),
      )

    for (cond <- conditions) {
      reverseString(cond._1)
      assert(cond._1 sameElements cond._2)
    }

  }

  test("Valid Palindrome II") {
    val conditions: Array[(String, Boolean)] =
      Array(
        ("aba", true),
        ("abca", true),
        ("abc", false),
        ("acbdba", true),
        ("abacadadarcaba", true),
        ("rabacadadacaba", true),
        ("rabacadadacababr", true),
        ("rabacadadacabbabr", false),
        ("abacadadarcabra", false),
        ("ebcbbececabbacecbbcbe", true),
        ("macccc", false)
      )

    for (cond <- conditions) {
      assert(validPalindromeTwo(cond._1) == cond._2)
    }

  }

  test("Next Permutation") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(1, 2, 3, 4, 5), Array(1, 2, 3, 5, 4)),
        (Array(1, 2, 3, 5, 4), Array(1, 2, 4, 3, 5)),
        (Array(1, 2, 4, 3, 5), Array(1, 2, 4, 5, 3)),
        (Array(1, 2, 4, 5, 3), Array(1, 2, 5, 3, 4)),
        (Array(1, 2, 5, 3, 4), Array(1, 2, 5, 4, 3)),
        (Array(1, 2, 5, 4, 3), Array(1, 3, 2, 4, 5)),
        (Array(1, 3, 5, 4, 2), Array(1, 4, 2, 3, 5)),
        (Array(1, 5, 4, 3, 2), Array(2, 1, 3, 4, 5)),
        (Array(2, 3, 5, 4, 1), Array(2, 4, 1, 3, 5)),
        (Array(5, 4, 3, 2, 1), Array(1, 2, 3, 4, 5)),
        (Array(1, 2, 3), Array(1, 3, 2)),
        (Array(3, 2, 1), Array(1, 2, 3)),
        (Array(1, 1, 5), Array(1, 5, 1)),
        (Array(1), Array(1)),
        (Array(1, 5, 1), Array(5, 1, 1)),
        (Array(5, 1, 1), Array(1, 1, 5)),
      )

    for (cond <- conditions) {
      nextPermutation(cond._1)
      assert(cond._1 sameElements cond._2)
    }
  }

  test("Merge Sorted Array") {
    val conditions: Array[(Array[Int], Array[Int], Array[Int])] =
      Array(
        (Array(1, 2, 3, 0, 0, 0), Array(2, 5, 6), Array(1, 2, 2, 3, 5, 6)),
        (Array(1), Array(), Array(1)),
        (Array(0), Array(1), Array(1)),
      )

    for (cond <- conditions) {
      merge(cond._1, cond._1.length - cond._2.length, cond._2, cond._2.length)
      assert(cond._1 sameElements cond._3)
    }
  }

  test("Brace Expansion") {
    val conditions: Array[(String, Array[String])] =
      Array(
        ("{a,b}c{d,e}f", Array("acdf", "acef", "bcdf", "bcef")),
        ("abcd", Array("abcd")),
        ("a{b,c}cd", Array("abcd", "accd")),
        ("ac{d,e}f", Array("acdf", "acef")),
        ("a", Array("a")),
        ("{a,b}{d,e,z}{r,d,e}f", Array("addf", "adef", "adrf", "aedf", "aeef", "aerf", "azdf", "azef", "azrf", "bddf", "bdef", "bdrf", "bedf", "beef", "berf", "bzdf", "bzef", "bzrf"))
      )

    for (cond <- conditions) {
      assert(expand(cond._1) sameElements cond._2, f"wrong for array: ${cond._1}")
    }
  }

  test("Shift 2D Grid") {
    val conditions: Array[(Array[Array[Int]], Int, List[List[Int]])] =
      Array(
        (Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), 1, List(List(9, 1, 2), List(3, 4, 5), List(6, 7, 8))),
        (Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), 2, List(List(8, 9, 1), List(2, 3, 4), List(5, 6, 7))),
        (Array(Array(3, 8, 1, 9), Array(19, 7, 2, 5), Array(4, 6, 11, 10), Array(12, 0, 21, 13)), 4, List(List(12, 0, 21, 13), List(3, 8, 1, 9), List(19, 7, 2, 5), List(4, 6, 11, 10))),
        (Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9), Array(-1, -2, -3)), 4, List(List(9, -1, -2), List(-3, 1, 2), List(3, 4, 5), List(6, 7, 8))),
        (Array(Array(1), Array(2), Array(3), Array(4), Array(7), Array(6), Array(5)), 23, List(List(6), List(5), List(1), List(2), List(3), List(4), List(7)))
      )

    for (cond <- conditions) {
      assert(shiftGrid(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._1.mkString}")
    }
  }

  test("Intersection of Two Arrays II") {
    val conditions: Array[(Array[Int], Array[Int], Array[Int])] =
      Array(
        (Array(1, 2, 2, 1), Array(2, 2), Array(2, 2)),
        (Array(4, 9, 5), Array(9, 4, 9, 8, 4), Array(4, 9)),
      )

    for (cond <- conditions) {
      intersect(cond._1, cond._2) should contain theSameElementsAs cond._3
    }
  }

  test("Count Prefixes of a Given String") {
    val conditions: Array[(Array[String], String, Int)] =
      Array(
        (Array("a", "b", "c", "ab", "bc", "abc"), "abc", 3),
        (Array("a", "a"), "aa", 2)
      )

    for (cond <- conditions) {
      assert(countPrefixes(cond._1, cond._2) == cond._3)
    }
  }

  test("Backspace String Compare") {
    val conditions: Array[(String, String, Boolean)] =
      Array(
        ("ab#c", "ad#c", true),
        ("ab##", "c#d#", true),
        ("a#c", "b", false),
        ("a##c", "#a#c", true),
        ("y#fo##f", "y#f#o##f", true)
      )

    for (cond <- conditions) {
      assert(backspaceCompareNaive(cond._1, cond._2) == cond._3)
    }
  }

  test("Shortest Unsorted Continuous Subarray") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(2, 6, 4, 8, 10, 9, 15), 5),
        (Array(2, 6, 7, 8, 10, 9, 15), 2),
        (Array(0, 7, 2, 6, 7, 8, 10, 9, 15), 7),
        (Array(1, 2, 3, 4), 0),
        (Array(1), 0),
        (Array(1, 2, 3, 3, 3), 0),
        (Array(1, 3, 2, 2, 2), 4),
        (Array(1, 3, 2, 3, 3), 2),
        (Array(2, 3, 3, 2, 4), 3),
        (Array(1, 2, 4, 5, 3), 3),
        (Array(1, 2, 2, 2, 3, 3, 4, 5, 3), 3),
        (Array(1, 2, 2, 2, 3, 3, 4, 5, 3), 3),
        (Array(1, 3, 3, 3, 3, 3, 4, 5, 3), 3),
        (Array(1, 2, 5, 3, 4), 3),
      )

    for (cond <- conditions) {
      assert(findUnsortedSubarray(cond._1) == cond._2)
    }
  }

  test("Find Permutation") {
    val conditions: Array[(String, Array[Int])] =
      Array(
        ("DDI", Array(3, 2, 1, 4)),
        ("I", Array(1, 2)),
        ("D", Array(2, 1)),
        ("DI", Array(2, 1, 3)),
        ("DID", Array(2, 1, 4, 3)),
        ("IDIDIDIDIDID", Array(1, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12)),
        ("DDDDDDDDDD", Array(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)),
        ("DIDIDIIDIDIIIDIIDIDIDIIIDDDDIDIDIDIIDIDIIDIDIIDIIDIDIDIDIDIIDIDIDIID", Array(2, 1, 4, 3, 6, 5, 7, 9, 8, 11, 10, 12, 13, 15, 14, 16, 18, 17, 20, 19, 22, 21, 23, 24, 29, 28, 27, 26, 25, 31, 30, 33, 32, 35, 34, 36, 38, 37, 40, 39, 41, 43, 42, 45, 44, 46, 48, 47, 49, 51, 50, 53, 52, 55, 54, 57, 56, 59, 58, 60, 62, 61, 64, 63, 66, 65, 67, 69, 68)),
        ("DIDIDIIDIIDIIDIDDDDDDDIDDIIDDDDDDDDDDD", Array(2, 1, 4, 3, 6, 5, 7, 9, 8, 10, 12, 11, 13, 15, 14, 23, 22, 21, 20, 19, 18, 17, 16, 26, 25, 24, 27, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28))
      )

    for (cond <- conditions) {
      assert(findPermutation(cond._1).toList == cond._2.toList)
    }
  }

  test("Remove All Adjacent Duplicates in String II") {
    val conditions: Array[(String, Int, String)] =
      Array(
        ("deeedbbcccbdaa", 3, "aa"),
        ("abcd", 2, "abcd"),
        ("pbbcggttciiippooaais", 2, "ps")
      )

    for (cond <- conditions) {
      assert(removeDuplicates(cond._1, cond._2) == cond._3)
    }
  }

  test("132 Pattern") {
    val conditions: Array[(Array[Int], Boolean)] =
      Array(
        (Array(1, 2, 3, 4), false),
        (Array(3, 1, 4, 2), true),
        (Array(-1, 3, 2, 0), true),
        (Array(1, 0, 1, -4, -3), false),
        (Array(3, 5, 0, 3, 4), true),
        (Array(1, 3, 2, 4, 5, 6, 7, 8, 9, 10), true),
        (Array(-2, 1, -1), true),
        (Array(-2, 1, 1), false),
      )

    for (cond <- conditions) {
      assert(find132pattern(cond._1) == cond._2)
    }
  }

  test("Largest 3-Same-Digit Number in String") {
    val conditions: Array[(String, String)] =
      Array(
        ("6777133339", "777"),
        ("2300019", "000"),
        ("42352338", ""),
        ("3200014888", "888"),
        ("222", "222"),
        ("74444", "444")
      )

    for (cond <- conditions) {
      assert(largestGoodInteger(cond._1) == cond._2)
    }
  }

  test("Letter Combinations of a Phone Number") {
    val conditions: Array[(String, List[String])] =
      Array(
        ("23", List("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")),
        ("2", List("a", "b", "c")),
        ("", List()),
        ("4444", List("gggg", "gggh", "gggi", "gghg", "gghh", "gghi", "ggig", "ggih", "ggii", "ghgg", "ghgh", "ghgi", "ghhg", "ghhh", "ghhi", "ghig", "ghih", "ghii", "gigg", "gigh", "gigi", "gihg", "gihh", "gihi", "giig", "giih", "giii", "hggg", "hggh", "hggi", "hghg", "hghh", "hghi", "hgig", "hgih", "hgii", "hhgg", "hhgh", "hhgi", "hhhg", "hhhh", "hhhi", "hhig", "hhih", "hhii", "higg", "high", "higi", "hihg", "hihh", "hihi", "hiig", "hiih", "hiii", "iggg", "iggh", "iggi", "ighg", "ighh", "ighi", "igig", "igih", "igii", "ihgg", "ihgh", "ihgi", "ihhg", "ihhh", "ihhi", "ihig", "ihih", "ihii", "iigg", "iigh", "iigi", "iihg", "iihh", "iihi", "iiig", "iiih", "iiii")),
        ("999", List("www", "wwx", "wwy", "wwz", "wxw", "wxx", "wxy", "wxz", "wyw", "wyx", "wyy", "wyz", "wzw", "wzx", "wzy", "wzz", "xww", "xwx", "xwy", "xwz", "xxw", "xxx", "xxy", "xxz", "xyw", "xyx", "xyy", "xyz", "xzw", "xzx", "xzy", "xzz", "yww", "ywx", "ywy", "ywz", "yxw", "yxx", "yxy", "yxz", "yyw", "yyx", "yyy", "yyz", "yzw", "yzx", "yzy", "yzz", "zww", "zwx", "zwy", "zwz", "zxw", "zxx", "zxy", "zxz", "zyw", "zyx", "zyy", "zyz", "zzw", "zzx", "zzy", "zzz")),
      )

    for (cond <- conditions) {
      assert(letterCombinations(cond._1) == cond._2)
    }
  }

  test("Combination Sum III") {
    val conditions: Array[(Int, Int, List[List[Int]])] =
      Array(
        (3, 7, List(List(1, 2, 4))),
        (3, 9, List(List(1, 2, 6), List(1, 3, 5), List(2, 3, 4))),
        (4, 1, List()),
        (7, 28, List(List(1, 2, 3, 4, 5, 6, 7))),
        (9, 60, List()),
        (4, 40, List()),
        (4, 15, List(List(1, 2, 3, 9), List(1, 2, 4, 8), List(1, 2, 5, 7), List(1, 3, 4, 7), List(1, 3, 5, 6), List(2, 3, 4, 6))),
        (5, 30, List(List(1, 5, 7, 8, 9), List(2, 4, 7, 8, 9), List(2, 5, 6, 8, 9), List(3, 4, 6, 8, 9), List(3, 5, 6, 7, 9), List(4, 5, 6, 7, 8))),
        (5, 40, List()),
        (7, 40, List(List(1, 4, 5, 6, 7, 8, 9), List(2, 3, 5, 6, 7, 8, 9))),
      )

    for (cond <- conditions) {
      assert(combinationSum3(cond._1, cond._2) == cond._3)
    }
  }

  test("Count Sorted Vowel Strings") {
    val conditions: Array[(Int, Int)] =
      Array(
        (1, 5),
        (2, 15),
        (25, 23751),
        (33, 66045),
        (50, 316251),
      )

    for (cond <- conditions) {
      assert(countVowelStrings(cond._1) == cond._2)
    }
  }

  test("Running Sum of 1d Array") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(1, 2, 3, 4), Array(1, 3, 6, 10)),
        (Array(1, 1, 1, 1, 1), Array(1, 2, 3, 4, 5)),
        (Array(3, 1, 2, 10, 1), Array(3, 4, 6, 16, 17)),
      )

    for (cond <- conditions) {
      assert(runningSum(cond._1).toList == cond._2.toList)
    }
  }

  test("Transpose Matrix") {
    val conditions: Array[(Array[Array[Int]], Array[Array[Int]])] =
      Array(
        (Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)), Array(Array(1, 4, 7), Array(2, 5, 8), Array(3, 6, 9))),
        (Array(Array(1, 2, 3), Array(4, 5, 6)), Array(Array(1, 4), Array(2, 5), Array(3, 6)))
      )

    for (cond <- conditions) {
      assert(transpose(cond._1).map(_.toList).toList == cond._2.map(_.toList).toList)
    }
  }

  test("Sum of All Odd Length Subarrays") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(1, 4, 2, 5, 3), 58),
        (Array(10, 11, 12), 66),
        (Array(1, 2), 3)
      )

    for (cond <- conditions) {
      assert(sumOddLengthSubarrays(cond._1) == cond._2)
    }
  }

  test("Daily Temperatures") {
    val conditions: Array[(Array[Int], Array[Int])] =
      Array(
        (Array(73, 74, 75, 71, 69, 72, 76, 73), Array(1, 1, 4, 2, 1, 1, 0, 0)),
        (Array(30, 40, 50, 60), Array(1, 1, 1, 0)),
        (Array(30, 60, 90), Array(1, 1, 0)),
      )

    for (cond <- conditions) {
      assert(dailyTemperatures(cond._1) sameElements cond._2)
    }
  }

  test("String Compression") {
    val conditions: Array[(Array[Char], Int)] =
      Array(
        (Array('a', 'a', 'b', 'b', 'c', 'c', 'c'), 6),
        (Array('a', 'b', 'b', 'b', 'b', 'b', 'b'), 3),
        (Array('a', 'a', 'b', 'b', 'c', 'c', 'c'), 6),
        (Array.fill(100)('a'), 4),
        (Array.fill(100)('a') ++ Array.fill(50)('b'), 7),
        (Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'a', 'b', 'c'), 12)
      )

    for (cond <- conditions) {
      assert(compress(cond._1) == cond._2)
    }
  }

  test("Find the Index of the First Occurrence in a String") {
    val conditions: Array[(String, String, Int)] =
      Array(
        ("sadbutsad", "sad", 0),
        ("leetcode", "leeto", -1),
        ("aarna", "ar", 1),
      )

    for (cond <- conditions) {
      assert(strStr(cond._1, cond._2) == cond._3)
    }
  }

}
