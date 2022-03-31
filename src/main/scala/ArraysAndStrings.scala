import SortingAndSearching.tripletBinarySearch

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.math.{Pi, abs}

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

  def twoSumBuffer(nums: Array[Int], target: Int): Array[(Int, Int)] = {
    val st: mutable.Set[Int] = mutable.Set()
    val res: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()

    for (i <- nums.indices) {
      val thisNum: Int = nums(i)
      val thatNum: Int = target - thisNum
      if (st.contains(thatNum)) {
        val tup = (thatNum, thisNum)
        res += tup
      }
      st += thisNum
    }
    res.toArray
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

    val packagesMax = packages.max
    val firstBoxWaste: Array[(Int, Long, Long)] = packages
      .sorted
      .scanLeft((0L, 0L))((t, pack) => (t._1 + pack, pack))
      .zipWithIndex
      .map {
        case ((cumsum: Long, pack: Long), index: Int) => (index, pack, pack * index - cumsum)
      }

    var res = Long.MaxValue

    for (box <- boxes) {
      if (!(packagesMax > box.max)) {
        val boxesSorted = box.sorted
        var (j, prevBoxIndex, thisResidual: Long) = (0, 0, 0L)
        while (j < box.length) {
          val prevBox = firstBoxWaste(prevBoxIndex)

          val currentBoxIndex = tripletBinarySearch(firstBoxWaste, boxesSorted(j), prevBoxIndex)
          val currentBox = firstBoxWaste(currentBoxIndex)

          val thisWastedSpace: Long = currentBox._3 - prevBox._3 - (currentBox._2 - prevBox._2) * prevBox._1.toLong + (boxesSorted(j).toLong - currentBox._2) * (currentBox._1.toLong - prevBox._1.toLong)
          thisResidual += thisWastedSpace
          prevBoxIndex = currentBoxIndex
          j += 1
        }
        res = res min thisResidual
      }
    }
    if (res == Long.MaxValue) -1 else (res % (Math.pow(10, 9).toLong + 7)).toInt
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

  def removeDuplicates(nums: Array[Int]): Int = {
    var (start, end) = (0, 0)
    while (end < nums.length) {
      if (nums(start) != nums(end)) {
        start += 1
      }
      nums(start) = nums(end)
      end += 1
    }
    start + 1
  }

  def checkIfExist(arr: Array[Int]): Boolean = {
    val st: mutable.Set[Int] = mutable.Set[Int]()
    var flag = false
    var i = 0

    while (!flag & i < arr.length) {
      if (st.contains(arr(i) * 2) || ((arr(i) % 2 == 0) && st.contains(arr(i) / 2))) {
        flag = true
      }
      st += arr(i)
      i += 1
    }
    flag
  }

  def validMountainArray(arr: Array[Int]): Boolean = {
    val n = arr.length
    var i = 1
    if (n < 3 || arr(i) < arr(i - 1)) {
      return false
    }

    while (i < n - 1 && arr(i) > arr(i - 1)) {
      i += 1
    }

    while (i < n && arr(i) < arr(i - 1)) {
      i += 1
    }
    if (i == n) true else false
  }

  def intToRoman(num: Int): String = {
    val arabToRom = Map(1 -> "I", 5 -> "V", 10 -> "X", 50 -> "L", 100 -> "C", 500 -> "D", 1000 -> "M")
    var result: String = arabToRom(1000) * (num / 1000)
    val numString = num.toString
    val n = numString.length
    var power = (num - (num / 1000) * 1000).toString.length - 1

    while (power >= 0) {
      val digitInt = numString(n - power - 1).asDigit
      val multiplier = Math.pow(10, power).toInt
      if (digitInt == 4 || digitInt == 9) {
        result = result.concat(arabToRom(multiplier))
        result = result.concat(arabToRom((digitInt + 1) * multiplier))
      } else {
        result = result.concat(arabToRom(5 * multiplier) * (digitInt / 5))
        result = result.concat(arabToRom(multiplier) * (digitInt % 5))
      }
      power -= 1
    }
    result
  }

  def isValid(s: String): Boolean = {
    var stack: ListBuffer[Char] = ListBuffer()
    var (stackLength, leftLength) = (0, s.length)
    val map: Map[Char, Char] = Map(')' -> '(', ']' -> '[', '}' -> '{')
    var i = 0
    var flag = true

    while (flag && i < s.length) {
      if (!map.contains(s(i))) {
        stack += s(i)
        stackLength += 1
      } else if (stack.isEmpty || stack.last != map(s(i))) {
        flag = false
      } else {
        stack = stack.dropRight(1)
        stackLength -= 1
      }
      leftLength -= 1
      if (stackLength > leftLength) {
        flag = false
      }
      i += 1
    }
    if (stack.nonEmpty) false else flag
  }

  def replaceElements(arr: Array[Int]): Array[Int] = {
    var globalMax = -1
    for (i <- arr.length - 1 to 0 by -1) {
      val localMax = globalMax max arr(i)
      arr(i) = globalMax
      globalMax = localMax
    }
    arr
  }

  def moveZeroes(nums: Array[Int]): Unit = {
    var (zeroPointer, nonZeroPointer) = (0, 0)

    while (nonZeroPointer < nums.length) {
      if (nums(nonZeroPointer) != 0) {
        val tempVal = nums(zeroPointer)
        nums(zeroPointer) = nums(nonZeroPointer)
        nums(nonZeroPointer) = tempVal
      }
      nonZeroPointer += 1
      if (!(nums(zeroPointer) == 0)) zeroPointer += 1
    }
  }

  def sortArrayByParity(nums: Array[Int]): Array[Int] = {
    var (start, end) = (0, 0)

    while (end < nums.length) {
      if (nums(end) % 2 == 0) {
        val tempVal = nums(start)
        nums(start) = nums(end)
        nums(end) = tempVal
      }
      end += 1
      if (nums(start) % 2 == 0) {
        start += 1
      }
    }
    nums
  }

  def heightChecker(heights: Array[Int]): Int = {
    heights.sorted.zip(heights).count(t => t._1 != t._2)
  }

  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    var (globalMax, left, right, i) = (0, 0, 0, 0)

    while (i < nums.length) {
      if (nums(i) == 0) {
        left = right
        right = 0
      } else {
        right += 1
      }
      globalMax = globalMax max (left + right + 1)
      i += 1
    }
    globalMax min nums.length
  }

  def thirdMax(nums: Array[Int]): Int = {
    val trie: Array[Long] = Array.fill(3)(Long.MinValue)

    for (elem <- nums) {
      var keep: Long = elem
      for (i <- 0 to 2) {
        if (keep > trie(i)) {
          val newKeep = trie(i)
          trie(i) = keep
          keep = newKeep
        } else if (keep == trie(i)) {
          keep = Long.MinValue
        }
      }
    }
    if (trie(2) != Long.MinValue) trie(2).toInt else trie(0).toInt
  }

}
