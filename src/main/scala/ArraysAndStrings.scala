import SortingAndSearching.tripletBinarySearch

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.abs

object ArraysAndStrings {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val hash: mutable.HashMap[Int, Int] = mutable.HashMap()
    val solution: Array[Int] = Array.fill(2)(0)
    var flag: Boolean = false
    var i: Int = 0

    while (i < nums.length && !flag) {
      if (hash.contains(target - nums(i))) {
        flag = true
        solution(0) = hash(target - nums(i))
        solution(1) = i
      }
      hash(nums(i)) = i
      i += 1
    }
    solution
  }

  def twoSumReturn(nums: Array[Int], target: Int): Array[Int] = {
    val st: mutable.Map[Int, Int] = mutable.Map()
    for (i <- nums.indices) {
      if (st.contains(target - nums(i))) {
        return Array(st(target - nums(i)), i)
      }
      st += (nums(i) -> i)
    }
    Array()
  }

  def myAtoi(s: String): Int = {
    val regexp = raw"^\s*([-|+]{0,1}\d+).*".r
    s match {
      case regexp(x) => ((BigInt(x) min Int.MaxValue) max Int.MinValue).toInt
      case _ => 0
    }
  }

  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    /**
     * You are given an array of non-overlapping intervals where intervals[i] = [starti, endi] represent the start and the end of the ith interval
     * and intervals is sorted in ascending order by starti.
     * You are also given an interval newInterval = [start, end] that represents the start and end of another interval.
     *
     * Insert newInterval into intervals such that intervals is still sorted in ascending order by starti
     * and intervals still does not have any overlapping intervals (merge overlapping intervals if necessary).
     *
     * Return intervals after the insertion.
     * */

    val updatedInterval = newInterval
    val res: ArrayBuffer[Array[Int]] = ArrayBuffer()
    var i = 0

    while (i < intervals.length && updatedInterval(0) > intervals(i)(1)) {
      res += intervals(i)
      i += 1
    }

    while (i < intervals.length && updatedInterval(1) >= intervals(i)(0)) {
      updatedInterval(0) = updatedInterval(0) min intervals(i)(0)
      updatedInterval(1) = updatedInterval(1) max intervals(i)(1)
      i += 1
    }

    res += updatedInterval

    while (i < intervals.length) {
      res += intervals(i)
      i += 1
    }

    res.toArray
  }

  def findPairs(nums: Array[Int], k: Int): Int = {
    /**
     * Given an array of integers nums and an integer k, return the number of unique k-diff pairs in the array.
     *
     * A k-diff pair is an integer pair (nums[i], nums[j]), where the following are true:
     *
     * 0 <= i, j < nums.length
     * i != j
     * nums[i] - nums[j] == k
     * Notice that |val| denotes the absolute value of val.
     * */
    val res: mutable.Set[(Int, Int)] = mutable.Set()
    val diffs: mutable.Set[Int] = mutable.Set()

    for (i <- nums.indices) {
      if (diffs.contains(nums(i) - k)) {
        val minVal = nums(i) min (nums(i) - k)
        val maxVal = nums(i) max (nums(i) - k)
        val pair = (minVal, maxVal)
        res += pair
      }
      if (diffs.contains(nums(i) + k)) {
        val minVal = nums(i) min (nums(i) + k)
        val maxVal = nums(i) max (nums(i) + k)
        val pair = (minVal, maxVal)
        res += pair
      }
      diffs += nums(i)
    }
    res.toSeq.length
  }

  def minWastedSpaceBruteForce(packages: Array[Int], boxes: Array[Array[Int]]): Int = {
    var res = Int.MaxValue
    var i = 0
    val packagesSorted = packages.sorted
    val n = packages.length

    while (i < boxes.length) {
      if (!(packages.max > boxes(i).max)) {
        val boxesSorted = boxes(i).sorted
        var (j, m, thisResidual) = (0, 0, 0)
        while (j < n) {
          if (packagesSorted(j) <= boxesSorted(m)) {
            thisResidual += boxesSorted(m) - packagesSorted(j)
            j += 1
          } else {
            m += 1
          }
        }
        res = res min thisResidual
      }
      i += 1
    }
    if (res == Int.MaxValue) -1 else (res % (Math.pow(10, 9) + 7)).toInt
  }

  def minWastedSpace(packages: Array[Int], boxes: Array[Array[Int]]): Int = {

    val firstBoxWaste = packages
      .sorted
      .scanLeft((0, 0))((t, pack) => (t._1 + pack, pack))
      .zipWithIndex
      .map(t => (t._2, t._1._2, t._1._2 * t._2 - t._1._1))

    var res = Int.MaxValue

    for (box <- boxes) {
      if (!(packages.max > box.max)) {
        val boxesSorted = box.sorted
        var (j, prevBoxIndex, thisResidual) = (0, 0, 0)
        while (j < box.length) {
          val prevBox = firstBoxWaste(prevBoxIndex)

          val currentBoxIndex = tripletBinarySearch(firstBoxWaste, boxesSorted(j))
          val currentBox = firstBoxWaste(currentBoxIndex)

          val thisWastedSpace = currentBox._3 - prevBox._3 - (currentBox._2 - prevBox._2) * prevBox._1 + (boxesSorted(j) - currentBox._2) * (currentBox._1 - prevBox._1)
          thisResidual += thisWastedSpace
          prevBoxIndex = currentBox._1
          j += 1
        }
        res = res min thisResidual
      }
    }
    if (res == Int.MaxValue) -1 else (res % (Math.pow(10, 9) + 7)).toInt
  }

  def lengthOfLongestSubstring(s: String): Int = {
    val n = s.length
    var (localMax, globalMax) = (0, 0)
    var i = 0
    val st: mutable.HashMap[Char, Int] = mutable.HashMap()

    while (i < n) {
      if (i - localMax > st.getOrElse(s(i), Int.MinValue)) {
        localMax += 1
        globalMax = globalMax max localMax
      } else {
        localMax = i - st(s(i))
      }
      st(s(i)) = i
      i += 1
    }
    globalMax
  }

  def maxAreaBruteForce(height: Array[Int]): Int = {
    /**
     * Trivial.
     *
     * Time complexity: O(n ** 2)
     * */
    var maxVal = 0

    for (i <- height.indices) {
      for (j <- i until height.length) {
        maxVal = maxVal max ((height(i) min height(j)) * abs(i - j))
      }
    }
    maxVal
  }

  def maxArea(height: Array[Int]): Int = {
    /** *
     * Time complexity: O(n)
     * */

    def squareF(left: Int, right: Int): Int = {
      (height(left) min height(right)) * (right - left)
    }

    var start = 0
    var end = height.length - 1
    var res = squareF(start, end)

    while (start < end) {
      if (height(start) <= height(end)) {
        start += 1
      } else {
        end -= 1
      }
      res = res max squareF(start, end)
    }
    res
  }

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    val res = nums.count(x => x != `val`)
    for ((elem, i) <- nums.filter(x => x != `val`).zipWithIndex) {
      nums(i) = elem
    }
    res
  }
}
