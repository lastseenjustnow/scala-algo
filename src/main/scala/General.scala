import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.abs

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
}
