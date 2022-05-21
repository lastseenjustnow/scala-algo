import SortingAndSearching.{searchInsert, searchInsertFirst}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{abs, max, min}

object General {
  def assignBikes(workers: Array[Array[Int]], bikes: Array[Array[Int]]): Array[Int] = {

    val workersIds = workers.zipWithIndex
    val bikesIds = bikes.zipWithIndex
    val distancesSorted = workersIds
      .flatMap(
        worker => bikesIds
          .map(bike => (abs(worker._1(0) - bike._1(0)) + abs(worker._1(1) - bike._1(1)), worker._2, bike._2))
      ).sorted.map(x => (x._2, x._3))

    val bikesTaken = Array.fill(bikes.length)(false)
    val workersAssigned = Array.fill(workers.length)(-1)
    var workersReceivedBike = 0
    var i = 0

    while (workersReceivedBike != workers.length) {
      val currentRule = distancesSorted(i)
      if (workersAssigned(currentRule._1) == -1 & !bikesTaken(currentRule._2)) {
        workersAssigned(currentRule._1) = currentRule._2
        bikesTaken(currentRule._2) = true
        workersReceivedBike += 1
      }
      i += 1
    }
    workersAssigned
  }


  def divideArray(nums: Array[Int]): Boolean = {
    val st: mutable.Set[Int] = mutable.Set[Int]()
    for (number <- nums) {
      if (st.contains(number)) {
        st -= number
      } else {
        st += number
      }
    }
    if (st.isEmpty) true else false
  }

  def maximumSubsequenceCount(text: String, pattern: String): Long = {
    var (bal: Long, left: Long, right: Long, maxLeftRight: Long) = (0L, 0L, 0L, 0L)
    for (ch <- text) {
      if (ch == pattern(0)) {
        left += 1
      }
      if (ch == pattern(1)) {
        bal += (if (pattern(0) != pattern(1)) left else left - 1)
        right += 1
      }
      maxLeftRight = Math.max(left, right)
    }
    bal + maxLeftRight
  }

  def halveArray(nums: Array[Int]): Int = {
    val numsDoubles = nums.map(_.toDouble).sum
    var (initialSum, currentSum) = (numsDoubles, numsDoubles)
    val stack: mutable.Queue[Double] = mutable.Queue()
    val sortedNums: mutable.Queue[Int] = mutable.Queue() ++ nums.sorted(Ordering.Int.reverse)
    var iterNum = 0

    while (sortedNums.nonEmpty & currentSum > initialSum / 2) {
      val elem: Double = if (stack.isEmpty || stack.head < sortedNums.head) sortedNums.dequeue() else stack.dequeue()
      val halfSumElem = elem / 2
      currentSum -= halfSumElem
      stack.enqueue(halfSumElem)
      iterNum += 1
    }
    iterNum
  }


  def twoSumSortedRecursive(numbers: Array[Int], target: Int): Array[Int] = {
    /** Given a 1-indexed array of integers numbers that is already sorted in non-decreasing order,
     * find two numbers such that they add up to a specific target number.
     *
     * Let these two numbers be numbers[index1] and numbers[index2] where 1 <= index1 < index2 <= numbers.length.
     *
     * Return the indices of the two numbers, index1 and index2, added by one as an integer array [index1, index2] of length 2.
     *
     * The tests are generated such that there is exactly one solution. You may not use the same element twice.
     *
     * Your solution must use only constant extra space.
     *
     */
    @tailrec
    def rec(start_i: Int, end_i: Int): Array[Int] = {
      (start_i, end_i) match {
        case (start_i, end_i) if numbers(start_i) + numbers(end_i) == target => Array(start_i + 1, end_i + 1)
        case (start_i, end_i) if numbers(end_i) > target - numbers(start_i) => rec(start_i, end_i - 1)
        case _ => rec(start_i + 1, end_i)
      }
    }

    rec(0, numbers.length - 1)
  }

  def twoSumSortedIterative(numbers: Array[Int], target: Int): Array[Int] = {
    var (start_i, end_i, flag) = (0, numbers.length - 1, false)
    while (!flag && start_i + 1 != end_i) {
      if (numbers(start_i) + numbers(end_i) == target) {
        flag = true
      } else if (numbers(end_i) > target - numbers(start_i)) {
        end_i -= 1
      } else start_i += 1
    }
    Array(start_i + 1, end_i + 1)
  }

  def threeSumTwoPointers(nums: Array[Int]): List[List[Int]] = {

    /**
     * Given an integer array nums, return all the triplets nums[i], nums[j], nums[k]
     * such that i != j, i != k, and j != k,
     * and nums[i] + nums[j] + nums[k] == 0.
     *
     * Notice that the solution set must not contain duplicate triplets.
     *
     * Two-pointers approach
     * Time complexity: O(n ** 2)
     * */
    val sortedNums = nums.sorted
    val n = nums.length
    val res: mutable.Set[List[Int]] = mutable.Set()
    var i = 0

    while (i < n - 1 && sortedNums(i) <= 0) {
      var (leftPointer, rightPointer) = (i + 1, n - 1)
      val target = 0 - sortedNums(i)
      while (leftPointer < rightPointer) {
        val twoNumsTarget = sortedNums(leftPointer) + sortedNums(rightPointer)
        if (twoNumsTarget == target) {
          res += List(sortedNums(i), sortedNums(leftPointer), sortedNums(rightPointer))
          leftPointer += 1
          rightPointer -= 1
        } else if (twoNumsTarget < target) {
          leftPointer += 1
        } else {
          rightPointer -= 1
        }
      }
      i += 1
    }
    res.toList
  }

  def threeSumHashSet(nums: Array[Int]): List[List[Int]] = {
    /**
     * Hashset approach
     * Time complexity: O(n ** 2)
     * */
    val sortedNums = nums.sorted
    val n = nums.length
    val res: mutable.Set[List[Int]] = mutable.Set()
    var i = 0

    while (i < n - 1 && sortedNums(i) <= 0) {
      val hashset: mutable.Set[Int] = mutable.Set()
      val target = 0 - sortedNums(i)
      var j = i + 1
      while (j < n && sortedNums(i) + sortedNums(j) <= target) {
        if (hashset.contains(target - sortedNums(j))) {
          res += List(sortedNums(i), target - sortedNums(j), sortedNums(j))
        } else {
          hashset += sortedNums(j)
        }
        j += 1
      }
      i += 1
    }
    res.toList
  }

  def threeSumNoSort(nums: Array[Int]): List[List[Int]] = {
    /**
     * Hashset approach
     * Time complexity: O(n ** 2)
     * */
    val n = nums.length
    val res: mutable.Set[List[Int]] = mutable.Set()
    var i = 0

    while (i < n - 1) {
      val hashset: mutable.Set[Int] = mutable.Set()
      val target = 0 - nums(i)
      var j = i + 1
      while (j < n) {
        if (hashset.contains(target - nums(j))) {
          res += List(nums(i), target - nums(j), nums(j)).sorted
        } else {
          hashset += nums(j)
        }
        j += 1
      }
      i += 1
    }
    res.toList
  }

  def threeSumMulti(arr: Array[Int], target: Int): Int = {

    val mapCounts = arr.groupBy(identity).mapValues(_.length)
    val sortedDistincts = mapCounts.keys.toArray.sorted

    def choose(n: Int, k: Int): BigInt = {
      def fact(n: Int): BigInt = (for (i <- 1 to n) yield BigInt(i)).product

      fact(n) / (fact(n - k) * fact(k))
    }

    var res: BigInt = 0

    // add triplets
    if (target % 3 == 0) {
      res = res + choose(mapCounts.getOrElse(target / 3, 0), 3)
    }

    var i = 0

    while (i < sortedDistincts.length && sortedDistincts(i) < target) {

      // add doublets
      val diff = target - sortedDistincts(i) * 2
      if (diff != sortedDistincts(i)) res = res + choose(mapCounts(sortedDistincts(i)), 2) * BigInt(mapCounts.getOrElse(diff, 0))

      var (j, k) = (i + 1, sortedDistincts.length - 1)
      while (j < k) {
        if (sortedDistincts(i) + sortedDistincts(j) + sortedDistincts(k) == target) {
          res = res + Array(i, j, k).map(x => mapCounts(sortedDistincts(x))).product
          k -= 1
        }
        else if (sortedDistincts(i) + sortedDistincts(j) + sortedDistincts(k) > target) k -= 1
        else j += 1
      }
      i += 1
    }
    (res % (Math.pow(10, 9) + 7).toInt).toInt
  }

  def findClosestNumber(nums: Array[Int]): Int = {
    var res = Int.MaxValue
    for (elem <- nums) {
      if (abs(elem) < abs(res) || (abs(elem) == abs(res) && elem > res))
        res = elem
    }
    res
  }

  def minimumAverageDifference(nums: Array[Int]): Int = {
    var (i, n, leftSum: Long, rightSum: Long) = (0, nums.length, 0L, nums.map(_.toLong).sum)
    var res: Long = 0L
    var minDif = Long.MaxValue

    while (i < nums.length) {
      leftSum += nums(i)
      rightSum -= nums(i)
      val leftCount = i + 1
      val rightCount = n - i - 1
      val diff: Long = Math.abs((leftSum / leftCount) - (if (rightCount == 0) 0 else rightSum / rightCount))
      if (diff < minDif) {
        res = i
        minDif = diff
      }
      i += 1
    }
    res.toInt
  }

  def minimumAverageDifferenceFunctional(nums: Array[Int]): Any = {
    val n = nums.length
    if (n == 0) return 0
    nums
      .scanLeft((0L, nums.map(_.toLong).sum))((x, y) => (x._1 + y, x._2 - y))
      .zipWithIndex
      .tail
      .map(x => (Math.abs(x._1._1 / x._2 - (if (n - x._2 == 0) 0 else x._1._2 / (n - x._2))), x._2 - 1))
      .min
      ._2
  }

  def divisorSubstrings(num: Int, k: Int): Int = {

    val ns = num.toString
    (0 to ns.length - k).map(
      x => num % (if (ns.substring(x, x + k).toInt != 0) ns.substring(x, x + k).toInt else num + 1)
    ).count(x => x == 0)

  }

  def waysToSplitArray(nums: Array[Int]): Int = {
    nums
      .scanLeft((0L, nums.map(_.toLong).sum))((x, y) => (x._1 + y, x._2 - y))
      .drop(1)
      .dropRight(1)
      .count(x => x._1 >= x._2)
  }

//  def maximumWhiteTiles(tiles: Array[Array[Int]], carpetLen: Int): Int = {
//    val n = tiles.length
//    val ts = tiles.map(x => (x(0), x(1))).sorted
//    val startPos = ts.map(_._1)
//    val preSum = Array.fill(n + 1)(0)
//    for (i <- 1 until n + 1) preSum(i) = preSum(i - 1) + (ts(i - 1)._2 - ts(i - 1)._1 + 1)
//
//    var res = 0
//
//    for (i <- 0 until n) {
//      val tile = ts(i)
//      if (tile._2 >= tile._1 + carpetLen - 1) return carpetLen
//      val endIdx = searchInsertFirst(startPos, tile._1 + carpetLen - 1) min (n - 1)
//      var compensate = 0
//      if (ts(endIdx)._2 > tile._1 + carpetLen - 1) compensate = ts(endIdx)._2 - tile._1 - carpetLen + 1
//      res = max(res, preSum(endIdx + 1) - preSum(i) - compensate)
//    }
//    res
//  }

}
