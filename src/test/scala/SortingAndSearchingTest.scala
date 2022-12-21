import SortingAndSearching._
import org.scalatest.FunSuite

class SortingAndSearchingTest extends FunSuite {
  test("Pow(x, n)") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 2, 1, 5, 6, 4), 2, 5),
        (Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4, 4),

      )

    val Eps = 1e-7

    for (cond <- conditions) {
      assert(findKthLargest(cond._1, cond._2) == cond._3)
    }
  }


  test("Binary Search") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(-1, 0, 3, 5, 9, 12), 9, 4),
        (Array(-1, 0, 3, 5, 9, 12), 2, -1),
        (Array(-1, 0, 3, 5, 9, 12), 3, 2),
        (Array(-1, 0, 3, 5, 9, 12), -1, 0),
        (Array(-1, 0, 3, 5, 6, 9, 12), 6, 4),
        (Array(-1, 0, 3, 5, 6, 9, 12), 12, 6),
        (Array(-1, 0, 3, 5, 6, 9, 12), 9, 5),
        (Array(-1, 0, 3, 5, 6, 9, 12), 0, 1),
        (Array(-4, -1, 0, 3, 5, 6, 9, 12), 0, 2)
      )

    for (cond <- conditions) {
      assert(search(cond._1, cond._2) == cond._3)
    }
  }

  test("Search Insert Position") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(1, 3, 5, 6), 5, 2),
        (Array(1, 3, 5, 6), 7, 4),
        (Array(1, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 0, 0),
        (Array(1, 2, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 6, 4),
        (Array(1, 2, 3, 5, 6), 4, 3),
        (Array(1, 2, 3, 5, 6), 3, 2),
        (Array(1, 2, 2, 2, 3, 5, 6), 2, 3),
        (Array(1, 2, 2, 2, 3, 5, 6), 5, 5)

      )

    for (cond <- conditions) {
      assert(searchInsert(cond._1, cond._2) == cond._3)
    }
  }

  test("Search First Insert Position") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 5), 2, 0),
        (Array(1, 3, 5, 6), 5, 2),
        (Array(1, 3, 5, 6), 7, 4),
        (Array(1, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 0, 0),
        (Array(1, 2, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 6, 4),
        (Array(1, 2, 3, 5, 6), 3, 2),
        (Array(1, 2, 2, 2, 3, 5, 6), 2, 1),
        (Array(1, 2, 2, 2, 3, 5, 6), 5, 5),
        (Array(1, 2, 4, 6, 8, 11, 14, 19, 21), 12, 6)
      )

    for (cond <- conditions) {
      assert(searchInsertFirst(cond._1, cond._2) == cond._3)
    }
  }

  test("First Bigger Than") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 5), 2, 0),
        (Array(1, 3, 5, 6), 5, 3),
        (Array(2, 3), 2, 1),
        (Array(1, 3), 3, 2),
        (Array(3), 3, 1),
        (Array(1, 3, 5, 6), 7, 4),
        (Array(1, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 0, 0),
        (Array(1, 2, 3, 5, 6), 2, 2),
        (Array(1, 2, 3, 5, 6), 6, 5),
        (Array(1, 2, 3, 5, 6), 3, 3),
        (Array(1, 2, 2, 2, 3, 5, 6), 2, 4),
        (Array(1, 2, 2, 2, 3, 5, 6), 5, 6),
        (Array(1, 2, 4, 6, 8, 11, 14, 19, 21), 12, 6)
      )

    for (cond <- conditions) {
      assert(firstBiggerThan(cond._1, cond._2) == cond._3)
    }
  }

  test("Peak Index in a Mountain Array") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(0, 1, 0), 1),
        (Array(0, 2, 1, 0), 1),
        (Array(0, 10, 5, 0), 1),
        (Array(3, 4, 5, 1), 2),
        (Array(3, 5, 3, 2, 0), 1)
      )

    for (cond <- conditions) {
      assert(peakIndexInMountainArray(cond._1) == cond._2)
    }
  }

  test("Binary Search Triplets") {
    val conditions: Array[(Array[(Int, Long, Long)], Int, Int)] =
      Array(
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 5, 3),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 6, 4),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 8, 5),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 9, 23), (8, 10, 29), (9, 12, 43)), 8, 6),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 9, 23), (8, 10, 29), (9, 12, 43)), 8, 6),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 9, 23), (8, 10, 29), (9, 12, 43)), 8, 6),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 8, 18), (8, 8, 18), (9, 8, 18), (10, 8, 18), (11, 9, 23), (12, 10, 29), (13, 12, 43)), 8, 10),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 12, 8),
        ((for (i <- 0 to 1000000) yield (i, i.toLong, i.toLong)).toArray, 500000, 500000)
      )

    for (cond <- conditions) {
      assert(tripletBinarySearch(cond._1, cond._2, 0) == cond._3)
    }
  }

  test("Insertion sort of a part of an array") {
    val conditions: Array[(Array[Int], Int, Array[Int])] =
      Array(
        (Array(1, 2, 4, 3, 5, 6, 1, 3, 2), 3, Array(1, 2, 4, 1, 2, 3, 3, 5, 6)),
        (Array(1, 2, 4, 3, 5, 6, 1, 3, 2), 4, Array(1, 2, 4, 3, 1, 2, 3, 5, 6))
      )

    for (cond <- conditions) {
      insertionSort(cond._1, cond._2)
      assert(cond._1 sameElements cond._3)
    }
  }

  test("Valid Perfect Square") {
    val conditions: Array[(Int, Boolean)] =
      Array(
        (16, true),
        (14, false),
        (30858025, true),
      )

    for (cond <- conditions) {
      assert(isPerfectSquare(cond._1) == cond._2)
    }
  }

  test("Find the Distance Value Between Two Arrays") {
    val conditions: Array[(Array[Int], Array[Int], Int, Int)] =
      Array(
        (Array(4, 5, 8), Array(10, 9, 1, 8), 2, 2),
        (Array(1, 4, 2, 3), Array(-4, -3, 6, 10, 20, 30), 3, 2),
        (Array(2, 1, 100, 3), Array(-5, -2, 10, -3, 7), 6, 1)
      )

    for (cond <- conditions) {
      assert(findTheDistanceValue(cond._1, cond._2, cond._3) == cond._4)
    }
  }

  test("My Sqrt(x)") {
    val conditions: Array[(Int, Int)] =
      Array(
        (0, 0), (1, 1), (2, 1), (3, 1), (4, 2), (5, 2), (8, 2), (9, 3), (25, 5), (26, 5)
      )

    for (cond <- conditions) {
      assert(mySqrt(cond._1) == cond._2)
    }
  }

  test("Find Smallest Letter Greater Than Target") {
    val conditions: Array[(Array[Char], Char, Char)] =
      Array(
        (Array('c', 'f', 'j'), 'a', 'c'),
        (Array('c', 'f', 'j'), 'c', 'f'),
        (Array('c', 'f', 'j'), 'd', 'f'),
        (Array('a', 'b'), 'z', 'a'),
      )

    for (cond <- conditions) {
      assert(nextGreatestLetter(cond._1, cond._2) == cond._3)
    }
  }

  test("Find First and Last Position of Element in Sorted Array") {

    val conditions: Array[(Array[Int], Int, Array[Int])] = Array(
      (Array(5, 7, 7, 8, 8, 10), 8, Array(3, 4)),
      (Array(5, 7, 7, 8, 8, 10), 6, Array(-1, -1)),
      (Array(), 1, Array(-1, -1)),
      (Array(5, 7, 7, 8, 8, 10, 10, 10, 10, 10), 10, Array(5, 9)),
      (Array(5, 7, 7, 8, 8, 10, 10, 10, 10, 10), 5, Array(0, 0)),
      (Array(5, 7, 7, 8, 8, 10, 10, 10, 10, 10), 7, Array(1, 2)),
    )

    for (cond <- conditions) {
      assert(searchRange(cond._1, cond._2).toList == cond._3.toList)
    }
  }

  test("Kth Missing Positive Number") {

    val conditions: Array[(Array[Int], Int, Int)] = Array(
      (Array(2, 3, 4, 7, 11), 5, 9),
      (Array(2, 3, 4, 7, 11), 6, 10),
      (Array(2, 3, 4, 7, 11), 7, 12),
      (Array(2, 3, 4, 7, 11), 8, 13),
      (Array(1, 2, 3, 4), 2, 6),
      (Array(1), 10, 11)
    )

    for (cond <- conditions) {
      assert(findKthPositive(cond._1, cond._2) == cond._3)
    }
  }

  test("Special Array With X Elements Greater Than or Equal X") {

    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(3, 5), 2),
      (Array(0, 0), -1),
      (Array(0, 4, 3, 0, 4), 3),
      (Array(3, 6, 7, 7, 0), -1)
    )

    for (cond <- conditions) {
      assert(specialArray(cond._1) == cond._2)
    }
  }

  test("Sum of Square Numbers") {

    val conditions: Array[(Int, Boolean)] = Array(
      (0, true),
      (1, true),
      (2, true),
      (3, false),
      (5, true),
      (9, true),
      (1000000000, true)
    )

    for (cond <- conditions) {
      assert(judgeSquareSum(cond._1) == cond._2, f"wrong for: ${cond._1}")
    }
  }

  test("Maximum Distance Between a Pair of Values") {

    val conditions: Array[(Array[Int], Array[Int], Int)] = Array(
      (Array(55, 30, 5, 4, 2), Array(100, 20, 10, 10, 5), 2),
      (Array(2, 2, 2), Array(10, 10, 1), 1),
      (Array(30, 29, 19, 5), Array(25, 25, 25, 25, 25), 2)
    )

    for (cond <- conditions) {
      assert(maxDistanceIterative(cond._1, cond._2) == cond._3)
      assert(maxDistanceFunctional(cond._1, cond._2) == cond._3)
      assert(maxDistanceTwoPointers(cond._1, cond._2) == cond._3)
    }
  }

  test("Check if Array Is Sorted and Rotated") {

    val conditions: Array[(Array[Int], Boolean)] = Array(
      (Array(3, 4, 5, 1, 2), true),
      (Array(2, 1, 3, 4), false),
      (Array(1, 2, 3), true),
      (Array(1, 2, 1, 1), true),
      (Array(1, 1, 2, 1), true),
    )

    for (cond <- conditions) {
      assert(check(cond._1) == cond._2, f"wrong for: ${cond._1.mkString(",")}")
    }
  }

  test("Search in Rotated Sorted Array") {

    val conditions: Array[(Array[Int], Int, Int)] = Array(
      (Array(0, 2, 3, 5, 6, 7, 8, 9, 10, -4, -2, -1), 2, 1),
      (Array(0, 2, 3, 5, 6, 7, 8, 9, 10, -4, -2, -1), 3, 2),
      (Array(4, 5, 6, 7, 0, 1, 2), 0, 4),
      (Array(4, 5, 6, 7, 0, 1, 2), 3, -1),
      (Array(1), 0, -1),
      (Array(5, 1, 3), 5, 0),
      (Array(3, 1), 0, -1)
    )

    for (cond <- conditions) {
      assert(searchInRotatedArray(cond._1, cond._2) == cond._3)
    }
  }

  test("Find Minimum in Rotated Sorted Array") {

    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(3, 4, 5, 1, 2), 1),
      (Array(6, 7, 8, 1, 2, 3, 4, 5), 1),
      (Array(0, 1, 2, 3, 4, 5, 6, 7), 0),
      (Array(4, 5, 6, 7, 0, 1, 2), 0),
      (Array(11, 13, 15, 17), 11),
      (Array(1), 1),
    )

    for (cond <- conditions) {
      assert(findMin(cond._1) == cond._2)
    }
  }

  test("Minimum Size Subarray Sum") {

    val conditions: Array[(Int, Array[Int], Int)] = Array(
      (7, Array(2, 3, 1, 2, 4, 3), 2),
      (4, Array(1, 4, 4), 1),
      (11, Array(1, 1, 1, 1, 1, 1, 1, 1), 0),
    )

    for (cond <- conditions) {
      assert(minSubArrayLen(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._2.mkString(",")}")
    }
  }

  test("Valid Triangle Number") {

    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(2, 2, 3, 4), 3),
      (Array(4, 2, 3, 4), 4),
      (Array(1, 1, 3, 4), 0),
      (Array(1, 2, 3, 4, 5, 6), 7),
    )

    for (cond <- conditions) {
      assert(triangleNumber(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

}
