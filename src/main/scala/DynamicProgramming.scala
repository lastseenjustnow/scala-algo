import scala.collection.mutable

object DynamicProgramming {
  /**
   * Bottom up implementation
   * Time:  O(n)
   * Space: O(1)
   */
  def climbStairs(n: Int): Int = {
    n match {
      case x if x <= 0 => 0
      case 1 => 1
      case _ =>
        var steps: (Int, Int) = (1, 2)
        var i: Int = 2
        while (i < n) {
          steps = (steps._2, steps._1 + steps._2)
          i += 1
        }
        steps._2
    }
  }

  def climbStairsRecursive(n: Int): Int = {
    /**
     * Top down implementation
     * Time:  O(n)
     * Space: O(n)
     */
    def rec(n: Int, memo: Map[Int, Int]): (Int, Map[Int, Int]) = {
      (n, memo) match {
        case (n, memo) if memo.contains(n) => (memo(n), memo)
        case _ =>
          val firstTerm = rec(n - 1, memo)
          val secondTerm = rec(n - 2, firstTerm._2)
          (firstTerm._1 + secondTerm._1, secondTerm._2)
      }
    }

    n match {
      case x if x < 0 => 0
      case x if x < 3 => x
      case _ => rec(n, Map(1 -> 1, 2 -> 2))._1
    }
  }

  def rob(nums: Array[Int]): Int = {
    /**
     * Bottom up implementation
     * Time:  O(n)
     * Space: O(1)
     */
    nums.length match {
      case x if x <= 0 => 0
      case x if x == 1 => nums(0)
      case _ =>
        var arr: (Int, Int) = (nums(0), nums(0).max(nums(1)))
        for (i <- 2 until nums.length) {
          arr = (arr._2, arr._2.max(arr._1 + nums(i)))
        }
        arr._2
    }
  }

  def robRecursive(nums: Array[Int]): Int = {
    /**
     * Top down implementation
     * Time:  O(n)
     * Space: O(n)
     */
    def rec(i: Int, memo: Map[Int, Int]): (Int, Map[Int, Int]) = {
      (i, memo) match {
        case (i, memo) if memo.contains(i) => (memo(i), memo)
        case _ =>
          val firstTerm = rec(i - 1, memo)
          val secondTerm = rec(i - 2, firstTerm._2)
          val maxValue = firstTerm._1.max(secondTerm._1 + nums(i))
          (maxValue, secondTerm._2 + (i -> maxValue))
      }
    }

    nums.length match {
      case x if x <= 0 => 0
      case x if x == 1 => nums(0)
      case _ =>
        val baseCase: Map[Int, Int] = Map(0 -> nums(0), 1 -> nums(0).max(nums(1)))
        rec(nums.length - 1, baseCase)._1
    }
  }

  def rob2(nums: Array[Int]): Int = {
    /**
     * House robber II
     *
     * You are a professional robber planning to rob houses along a street.
     * Each house has a certain amount of money stashed.
     *
     * ''All houses at this place are arranged in a circle.
     * That means the first house is the neighbor of the last one.''
     *
     * Meanwhile, adjacent houses have a security system connected,
     * and it will automatically contact the police if two adjacent houses were broken into on the same night.
     *
     * Given an integer array nums representing the amount of money of each house,
     * return the maximum amount of money you  can rob tonight without alerting the police.
     *
     *
     * Bottom up implementation
     * Time:  O(n)
     * Space: O(1)
     *
     * Idea: last and the first one can't be included together - so only the biggest one will be considered to be added to the final result
     */
    val n = nums.length
    val no_first = nums.drop(1)
    val no_last = nums.take(n - 1)
    n match {
      case n if n == 1 => nums(0)
      case _ => rob(no_first) max rob(no_last)
    }
  }


  def minCostClimbingStairs(cost: Array[Int]): Int = {
    /**
     * Bottom up implementation
     * Time:  O(n)
     * Space: O(1)
     */
    cost.length match {
      case x if x < 1 => 0
      case x if x == 1 => cost(0)
      case _ =>
        var costs = (cost(0), cost(1))
        for (i <- 2 until cost.length) {
          costs = (costs._2, costs._1 + cost(i) min costs._2 + cost(i))
        }
        costs._1 min costs._2
    }
  }

  def minCostClimbingStairsRecursive(cost: Array[Int]): Int = {

    def rec(i: Int, memo: Map[Int, Int]): (Int, Map[Int, Int]) = {
      (i, memo) match {
        case (i, memo) if memo.contains(i) => (memo(i), memo)
        case _ =>
          val firstTerm = rec(i - 1, memo)
          val secondTerm = rec(i - 2, firstTerm._2)
          val maxValue = firstTerm._1 + cost(i) min secondTerm._1 + cost(i)
          (maxValue, secondTerm._2 + (i -> maxValue))
      }
    }

    cost.length match {
      case x if x <= 0 => 0
      case x if x == 1 => cost(0)
      case _ =>
        val baseCase: Map[Int, Int] = Map(0 -> cost(0), 1 -> cost(1))
        val result = rec(cost.length - 1, baseCase)
        result._1 min result._2(cost.length - 2)
    }
  }

  def fib(n: Int): Int = {
    var i = 2
    var m = (0, 1)
    while (i <= n) {
      m = (m._2, m._1 + m._2)
      i += 1
    }
    if (n <= 0) m._1 else m._2
  }

  def fibFP(n: Int): Int = {
    n match {
      case x if x <= 0 => 0
      case _ => (1 until n).foldLeft((0, 1))((x, _) => (x._2, x._1 + x._2))._2
    }
  }

  def tribonacci(n: Int): Int = {
    /**
     * Bottom up implementation
     * Time:  O(n)
     * Space: O(1)
     */
    n match {
      case x if x == 0 => 0
      case x if x < 3 => 1
      case _ =>
        var tri: (Int, Int, Int) = (0, 1, 1)
        for (_ <- 3 to n) {
          tri = (tri._2, tri._3, tri._1 + tri._2 + tri._3)
        }
        tri._3
    }
  }

  def tribonacciRecursive(n: Int): Int = {
    /**
     * Top down implementation
     * Time:  O(n)
     * Space: O(n)
     */
    def rec(i: Int, memo: Map[Int, Int]): (Int, Map[Int, Int]) = {
      (i, memo) match {
        case _ if memo.contains(i) => (memo(i), memo)
        case _ =>
          val firstVal = rec(i - 3, memo)
          val secondVal = rec(i - 2, firstVal._2)
          val thirdVal = rec(i - 1, secondVal._2)
          (firstVal._1 + secondVal._1 + thirdVal._1, thirdVal._2 + (i -> (firstVal._1 + secondVal._1 + thirdVal._1)))
      }
    }

    rec(n, Map(0 -> 0, 1 -> 1, 2 -> 1))._1
  }

  def deleteAndEarn(nums: Array[Int]): Int = {
    val sums = Array.fill(nums.max)(0)

    for (elem <- nums) {
      sums(elem - 1) += elem
    }
    rob(sums)
  }

  def maximumScoreRecursive(nums: Array[Int], multipliers: Array[Int]): Int = {
    /**
     * You are given two integer arrays nums and multipliers of size n and m respectively, where n >= m. The arrays are 1-indexed.
     *
     * You begin with a score of 0. You want to perform exactly m operations. On the ith operation (1-indexed), you will:
     *
     * - Choose one integer x from either the start or the end of the array nums.
     * - Add multipliers[i] * x to your score.
     * - Remove x from the array nums.
     *
     * Return the maximum score after performing m operations.
     *
     * Top down implementation
     * */
    val memo = new mutable.HashMap[(Int, Int), Int]()

    def rec(i: Int, start_i: Int): Int = {
      i match {
        case i if i == multipliers.length => 0
        case _ =>
          val left = memo.getOrElseUpdate((i + 1, start_i + 1), rec(i + 1, start_i + 1))
          val right = memo.getOrElseUpdate((i + 1, start_i), rec(i + 1, start_i))
          (left + nums(start_i) * multipliers(i)) max (right + nums(nums.length - 1 - (i - start_i)) * multipliers(i))
      }
    }

    rec(0, 0)
  }

  def maximumScore(nums: Array[Int], multipliers: Array[Int]): Int = {
    /**
     * Bottom up implementation
     * */
    val m = multipliers.length
    val memo: Array[Array[Int]] = Array.fill(m + 1, m + 1)(0)

    for (i <- m - 1 to 0 by -1) {
      for (left <- i to 0 by -1) {
        val mult = multipliers(i)
        val right = nums.length - 1 - (i - left)
        memo(i)(left) = memo(i + 1)(left + 1) + mult * nums(left) max memo(i + 1)(left) + mult * nums(right)
      }
    }
    memo(0)(0)
  }

  def longestCommonSubsequenceRecursive(text1: String, text2: String): Int = {
    /**
     * Given two strings text1 and text2, return the length of their longest common subsequence.
     * If there is no common subsequence, return 0.
     *
     * A subsequence of a string is a new string generated from the original string with some characters (can be none) deleted without changing the relative order of the remaining characters.
     *
     * For example, "ace" is a subsequence of "abcde".
     * A common subsequence of two strings is a subsequence that is common to both strings.
     *
     * Top-down implementation
     * */
    val memo = new mutable.HashMap[(Int, Int), Int]()

    def rec(left_i: Int, right_i: Int): Int = {
      (left_i, right_i) match {
        case (left_i, right_i) if left_i == text1.length || right_i == text2.length => 0
        case (left_i, right_i) if text1(left_i) == text2(right_i) => rec(left_i + 1, right_i + 1) + 1
        case _ =>
          val left = memo.getOrElseUpdate((left_i + 1, right_i), rec(left_i + 1, right_i))
          val right = memo.getOrElseUpdate((left_i, right_i + 1), rec(left_i, right_i + 1))
          if (text1(left_i) == text2(right_i)) 1 else 0 + left max right
      }
    }

    rec(0, 0)
  }

  def longestCommonSubsequence(text1: String, text2: String): Int = {
    /**
     * Bottom-up implementation
     * */
    val memo: Array[Array[Int]] = Array.fill(text1.length + 1, text2.length + 1)(0)

    for (i <- 1 to text1.length) {
      for (j <- 1 to text2.length) {
        memo(i)(j) = if (text1(i - 1) == text2(j - 1)) memo(i - 1)(j - 1) + 1 else memo(i - 1)(j) max memo(i)(j - 1)
      }
    }
    memo(text1.length)(text2.length)
  }

  def maximalSquareRecursive(matrix: Array[Array[Char]]): Int = {

    val memo = new mutable.HashMap[(Int, Int), (Int, Int)]()

    def rec(i: Int, j: Int): (Int, Int) = {
      (i, j) match {
        case (i, j) if i == 0 || j == 0 => (matrix(j)(i).asDigit, matrix(j)(i).asDigit)
        case _ =>
          val upper = memo.getOrElseUpdate((i, j - 1), rec(i, j - 1))
          val left = memo.getOrElseUpdate((i - 1, j), rec(i - 1, j))
          val upleft = memo.getOrElseUpdate((i - 1, j - 1), rec(i - 1, j - 1))
          val minVal = upper._1 min left._1 min upleft._1
          val this_sq = if (minVal > 0 && matrix(j)(i).asDigit == 1) minVal + 1 else matrix(j)(i).asDigit
          (this_sq, this_sq max upper._2 max left._2 max upleft._2)
      }
    }

    if (matrix.length == 1) {
      matrix(0).map(x => x.asDigit).max
    } else if (matrix(0).length == 1) {
      matrix.map(x => x.head.asDigit).max
    } else
      Math.pow(rec(matrix(0).length - 1, matrix.length - 1)._2, 2).toInt
  }

  def maximalSquare(matrix: Array[Array[Char]]): Int = {
    val memo: Array[Array[Int]] = matrix.clone().map(x => x.map(y => y.asDigit))
    val (m, n) = (matrix(0).length, matrix.length)
    var max_sq = if (matrix.exists(row => row.contains('1'))) 1 else 0

    for (j <- 1 until n) {
      for (i <- 1 until m) {
        val minVal = memo(j)(i - 1) min memo(j - 1)(i) min memo(j - 1)(i - 1)
        memo(j)(i) = if (matrix(j)(i).asDigit == 1 && minVal > 0) minVal + 1 else matrix(j)(i).asDigit
        max_sq = max_sq max memo(j)(i)
      }
    }
    if (matrix.length == 1) {
      matrix(0).map(x => x.asDigit).max
    } else if (matrix(0).length == 1) {
      matrix.map(x => x.head.asDigit).max
    } else
      Math.pow(max_sq, 2).toInt
  }

  def canJumpRecursive(nums: Array[Int]): Boolean = {
    /** You are given an integer array nums.
     * You are initially positioned at the array's first index,
     * and each element in the array represents your maximum jump length at that position.
     *
     * Return true if you can reach the last index, or false otherwise. */

    val memo = new mutable.HashMap[Int, Boolean]()

    def rec(i: Int): Boolean = {
      i match {
        case i if i >= nums.length - 1 => true
        case i if nums(i) == 0 => false
        case _ =>
          var flag = false
          var j = nums(i)
          while (j != 0 && !flag) {
            flag = memo.getOrElseUpdate(i + j, rec(i + j))
            j -= 1
          }
          flag
      }
    }

    rec(0)
  }

  def canJump(nums: Array[Int]): Boolean = {
    /** You are given an integer array nums.
     * You are initially positioned at the array's first index,
     * and each element in the array represents your maximum jump length at that position.
     *
     * Return true if you can reach the last index, or false otherwise. */
    val q: mutable.Queue[Int] = mutable.Queue(0)
    var set: Set[Int] = Set(0)
    var flag: Boolean = if (nums.length <= 1) true else false
    while (q.nonEmpty && !flag) {
      val currentIndex = q.dequeue()
      var j = currentIndex + nums(currentIndex)
      while (j >= currentIndex && !flag) {
        flag = if (j >= nums.length - 1) true else false
        if (!set.contains(j)) {
          q.enqueue(j)
          set = set + j
        }
        j -= 1
      }
    }
    flag
  }

  def canJump2(nums: Array[Int]): Int = {
    /** Given an array of non-negative integers nums, you are initially positioned at the first index of the array.
     *
     * Each element in the array represents your maximum jump length at that position.
     *
     * Your goal is to reach the last index in the minimum number of jumps.
     *
     * You can assume that you can always reach the last index. */

    val n = nums.length
    val seq = Array.fill(n)(0)
    var i = 0
    while (i != n - 1) {
      for (j <- i + 1 to (i + nums(i)).min(n - 1)) {
        seq(j) = if (seq(j) == 0) seq(i) + 1 else seq(j) min seq(i) + 1
      }
      i += 1
    }
    seq(n - 1)
  }

  def maxSubArray(nums: Array[Int]): Int = {
    /**
     * Given an integer array nums,
     * find the contiguous subarray (containing at least one number) which has the largest sum
     * and return its sum.
     *
     * A subarray is a contiguous part of an array.
     *
     * Time complexity: O(n)
     */
    var globalMax = nums(0)
    var i = 1
    while (globalMax <= 0 & i < nums.length) {
      globalMax = if (nums(i) > globalMax) nums(i) else globalMax
      i += 1
    }
    var localMax = globalMax
    while (i < nums.length) {
      localMax += nums(i)
      localMax = if (localMax < 0) 0 else localMax
      globalMax = if (localMax > globalMax) localMax else globalMax
      i += 1
    }
    globalMax
  }

  def maxSubArrayKadane(nums: Array[Int]): Int = {
    /**
     * Given an integer array nums,
     * find the contiguous subarray (containing at least one number) which has the largest sum
     * and return its sum.
     *
     * A subarray is a contiguous part of an array.
     *
     * Time complexity: O(n)
     */
    var (globalMax, localMax) = (nums(0), nums(0))
    for (i <- 1 until nums.length) {
      localMax = nums(i) max (nums(i) + localMax)
      globalMax = globalMax max localMax
    }
    globalMax
  }

  def minSubArrayKadane(nums: Array[Int]): Int = {
    /**
     * Given an integer array nums,
     * find the contiguous subarray (containing at least one number) which has the largest sum
     * and return its sum.
     *
     * A subarray is a contiguous part of an array.
     *
     * Time complexity: O(n)
     */
    var (globalMin, localMin) = (nums(0), nums(0))
    for (i <- 1 until nums.length) {
      localMin = nums(i) min (nums(i) + localMin)
      globalMin = globalMin min localMin
    }
    globalMin
  }

  def maxSubarraySumCircularBruteForce(nums: Array[Int]): Int = {
    /** Given a circular integer array nums of length n,
     * return the maximum possible sum of a non-empty subarray of nums.
     *
     * A circular array means the end of the array connects to the beginning of the array.
     * Formally, the next element of nums[i] is nums[(i + 1) % n] and the previous element of nums[i] is nums[(i - 1 + n) % n].
     *
     * A subarray may only include each element of the fixed buffer nums at most once.
     * Formally, for a subarray nums[i], nums[i + 1], ..., nums[j], there does not exist i <= k1, k2 <= j with k1 % n == k2 % n.
     *
     * Naive solution: O(n ** 2)
     */
    val kadanes = for (i <- nums.indices) yield maxSubArrayKadane(nums.drop(i) ++ nums.take(i))
    kadanes.max
  }

  def maxSubarraySumCircular(nums: Array[Int]): Int = {
    /** Idea: compase regular max kadane with excluded minimum subarray sum.
     * As the sum must contain at least one element added, to avoid extreme cases when minimum subset is the whole array,
     * I have to exclude first and then last element of the array.
     * */
    if (nums.length == 1) {
      nums.head
    } else {
      val maxKadane = maxSubArrayKadane(nums)
      val noFirstMinKadane = nums.sum - minSubArrayKadane(nums.drop(1))
      val noLastMinKadane = nums.sum - minSubArrayKadane(nums.take(nums.length - 1))
      maxKadane max noFirstMinKadane max noLastMinKadane
    }
  }

  def maxProductNaive(nums: Array[Int]): Int = {
    /** Given an integer array nums,
     * find a contiguous non-empty subarray within the array that has the largest product, and return the product.
     *
     * The test cases are generated so that the answer will fit in a 32-bit integer.
     *
     * A subarray is a contiguous subsequence of the array.
     *
     * First attempt solution, naive
     */
    def nonZeroSubarrayMaxProduct(arr: Array[Int]): Option[Int] = {
      val isEvenNegatives = arr.count(_ < 0) % 2
      val n = arr.length
      (isEvenNegatives, n) match {
        case (_, n) if n == 0 => None
        case (_, n) if n == 1 => Some(arr.head)
        case (x, _) if x == 0 => Some(arr.product)
        case _ =>
          val leftSubarray = arr.take(arr.lastIndexWhere(_ < 0)).product
          val rightSubarray = arr.drop(arr.indexWhere(_ < 0) + 1).product
          Some(leftSubarray max rightSubarray)
      }
    }

    var (i, maxProduct): (Int, Option[Int]) = (0, None)
    while (i <= nums.length) {
      val arr = nums.drop(i).takeWhile(_ != 0)
      val localMaxProduct = nonZeroSubarrayMaxProduct(arr)
      if (maxProduct.isEmpty || maxProduct.get < localMaxProduct.getOrElse(maxProduct.get)) {
        maxProduct = localMaxProduct
      }
      i += arr.length + 1
    }
    if (nums.length == 1) nums.head else maxProduct.get max 0
  }

  def maxProduct(nums: Array[Int]): Int = {
    var (max_so_far, min_so_far, result) = (nums(0), nums(0), nums(0))
    for (i <- 1 until nums.length) {
      val tempMax = nums(i) max (max_so_far * nums(i)) max (min_so_far * nums(i))
      min_so_far = nums(i) min (max_so_far * nums(i)) min (min_so_far * nums(i))
      max_so_far = tempMax
      result = result max max_so_far
    }
    result
  }

  def getMaxLen(nums: Array[Int]): Int = {
    def nonZeroSubarrayMaxLength(arr: Array[Int]): Int = {
      val isEvenNegatives = arr.count(_ < 0) % 2
      val n = arr.length
      (isEvenNegatives, n) match {
        case (_, n) if n == 1 => if (arr.head > 0) 1 else 0
        case (_, n) if n == 0 => 0
        case (x, _) if x == 0 => arr.length
        case _ =>
          val leftSubarray = arr.take(arr.lastIndexWhere(_ < 0)).length
          val rightSubarray = arr.drop(arr.indexWhere(_ < 0) + 1).length
          leftSubarray max rightSubarray
      }
    }

    var (i, maxLength): (Int, Int) = (0, 0)
    while (i <= nums.length) {
      val arr = nums.drop(i).takeWhile(_ != 0)
      val localLength = nonZeroSubarrayMaxLength(arr)
      if (maxLength == 0 || maxLength < localLength) {
        maxLength = localLength
      }
      i += arr.length + 1
    }
    maxLength
  }

  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    val hm = new mutable.HashMap[Int, List[String]]()

    for (str: String <- strs) {
      val alphabet: Array[Int] = Array.fill(26)(0)
      for (c: Char <- str) {
        alphabet(c.asDigit - 10) += 1
      }
      val hash = alphabet.toSeq.hashCode
      hm.get(hash) match {
        case None => hm(hash) = List(str)
        case _ => hm(hash) = hm(hash) :+ str
      }
    }
    hm.values.toList
  }

  def minDifficultyRecursive(jobDifficulty: Array[Int], d: Int): Int = {
    /** You want to schedule a list of jobs in d days.
     * Jobs are dependent (i.e To work on the ith job, you have to finish all the jobs j where 0 <= j < i).
     *
     * You have to finish at least one task every day.
     * The difficulty of a job schedule is the sum of difficulties of each day of the d days.
     * The difficulty of a day is the maximum difficulty of a job done on that day.
     *
     * You are given an integer array jobDifficulty and an integer d. The difficulty of the ith job is jobDifficulty[i].
     *
     * Return the minimum difficulty of a job schedule. If you cannot find a schedule for the jobs return -1.
     *
     * Top down implementation
     * */

    val n = jobDifficulty.length
    val hardestJobRemaining: Array[Int] = jobDifficulty.scanRight(jobDifficulty(n - 1))(_ max _).take(n)
    val memo = new mutable.HashMap[(Int, Int), Int]()

    def dp(i: Int, day: Int): Int = {
      (i, day) match {
        case (i, day) if day == d => hardestJobRemaining(i)
        case _ =>
          val sub = jobDifficulty.slice(i, n - (d - day)).zipWithIndex
          val results =
            sub
              .scan(sub(0))((a, b) => (a._1 max b._1, b._2 + i + 1))
              .drop(1)
              .map(x => x._1 + memo.getOrElseUpdate((x._2, day + 1), dp(x._2, day + 1)))
          results.min
      }
    }

    if (jobDifficulty.length >= d) dp(0, 1) else -1
  }

  def minDifficulty(jobDifficulty: Array[Int], d: Int): Int = {
    val n = jobDifficulty.length
    val memo: Array[Array[Double]] = Array.fill(d)(Array.fill(n)(Double.PositiveInfinity))
    memo(d - 1) = jobDifficulty.scanRight(jobDifficulty(n - 1))(_ max _).take(n).map(_.toDouble)

    for (day <- d - 2 to 0 by -1) {
      for (i <- day until n) {
        var hardest = jobDifficulty(i)
        for (j <- i until n - 1) {
          hardest = hardest max jobDifficulty(j)
          val nextDp = memo(day + 1)(j + 1)
          memo(day)(i) = memo(day)(i) min (hardest + nextDp)
        }
      }
    }
    if (jobDifficulty.length >= d) memo(0)(0).toInt else -1
  }

  def maxScoreSightseeingPair(values: Array[Int]): Int = {
    /** You are given an integer array values where values[i] represents the value of the ith sightseeing spot.
     * Two sightseeing spots i and j have a distance j - i between them.
     *
     * The score of a pair (i < j) of sightseeing spots is values[i] + values[j] + i - j:
     * the sum of the values of the sightseeing spots, minus the distance between them.
     *
     * Return the maximum score of a pair of sightseeing spots.
     *
     * Naive solution
     * Time complexity: O(n)
     * Space complexity: O(n)
     * */
    var (maxVal, secondMaxVal, result) = (values(0) - 1 max values(1), values(0) - 1 min values(1), values(0) + values(1) - 1)
    for (i <- 2 until values.length) {
      val tempMax = (maxVal - 1) max values(i)
      secondMaxVal = (secondMaxVal - 1) max ((maxVal - 1) min values(i))
      maxVal = tempMax
      result = result max (maxVal + secondMaxVal)
    }
    result
  }

  def maxProfitNaive(prices: Array[Int]): Int = {
    /** You are given an array prices where prices[i] is the price of a given stock on the ith day.
     *
     * You want to maximize your profit by choosing a single day to buy one stock and choosing a different day in the future to sell that stock.
     *
     * Return the maximum profit you can achieve from this transaction. If you cannot achieve any profit, return 0.
     *
     * */
    var (trough, peak, profit) = (prices(0), prices(0), 0)
    for (i <- 1 until prices.length) {
      trough = prices(i) min trough
      peak = prices(i) max trough
      profit = profit max (peak - trough)
    }
    profit
  }

  def maxProfitII(prices: Array[Int]): Int = {
    /**
     * You are given an integer array prices where prices[i] is the price of a given stock on the ith day.
     *
     * On each day, you may decide to buy and/or sell the stock.
     * You can only hold at most one share of the stock at any time. However, you can buy it then immediately sell it on the same day.
     *
     * Find and return the maximum profit you can achieve.
     *
     */
    var (trough, peak, profit, maxProfit) = (prices(0), prices(0), 0, 0)
    for (i <- 1 until prices.length) {
      if (prices(i) < peak) {
        maxProfit += profit
        trough = prices(i)
        peak = prices(i)
        profit = 0
      }
      trough = prices(i) min trough
      peak = prices(i) max trough
      profit = profit max (peak - trough)
    }
    maxProfit + profit
  }

  def maxProfitCooldown(prices: Array[Int]): Int = {
    var (sold, held, reset): (Double, Double, Double) = (Double.NegativeInfinity, Double.NegativeInfinity, 0)
    for (price <- prices) {
      val preSold = sold
      sold = held + price
      held = held max (reset - price)
      reset = reset max preSold
    }
    (sold max reset).toInt
  }

  def maxProfitKTransactionsRecursive(k: Int, prices: Array[Int]): Int = {
    /**
     * You are given an integer array prices where prices[i] is the price of a given stock on the ith day, and an integer k.
     *
     * Find the maximum profit you can achieve. You may complete at most k transactions.
     *
     * Note: You may not engage in multiple transactions simultaneously (i.e., you must sell the stock before you buy again).
     * */

    val memo: mutable.HashMap[(Boolean, Int, Int), Int] = mutable.HashMap()

    def dp(isBought: Boolean, remainingK: Int, i: Int): Int = {
      (isBought, remainingK, i) match {
        case (_, remainingK, i) if remainingK == 0 || i == prices.length => 0
        case _ =>
          val skip = dp(isBought, remainingK, i + 1)
          val buyOrSell = if (isBought)
            memo.getOrElseUpdate((false, remainingK - 1, i + 1), dp(isBought = false, remainingK - 1, i + 1) + prices(i))
          else {
            memo.getOrElseUpdate((true, remainingK, i + 1), dp(isBought = true, remainingK, i + 1) - prices(i))
          }
          buyOrSell max skip
      }
    }

    dp(isBought = false, k, 0)
  }

  def maxProfitKTransactionsIterative(k: Int, prices: Array[Int]): Int = {

    val n = prices.length
    val memo: Array[Array[Array[Int]]] = Array.fill(n + 1)(Array.fill(k + 1)(Array.fill(2)(0)))

    for (i <- n - 1 to 0 by -1) {
      for (remainingK <- 1 to k) {
        for (isBought <- 0 to 1) {
          val skip = memo(i + 1)(remainingK)(isBought)
          val buyOrSell = if (isBought == 1) memo(i + 1)(remainingK - 1)(0) + prices(i) else memo(i + 1)(remainingK)(1) - prices(i)
          memo(i)(remainingK)(isBought) = skip max buyOrSell
        }
      }
    }
    memo(0)(k)(0)
  }

  def coinChangeRecursive(coins: Array[Int], amount: Int): Int = {

    val memo = new mutable.HashMap[(Int, Int), Double]()

    def dp(i: Int, am: Int): Double = {
      (i, am) match {
        case (i, am) if i < 0 || am < 0 => Double.PositiveInfinity
        case (_, am) if am == 0 => 0
        case _ =>
          val left = memo.getOrElseUpdate((i - 1, am), dp(i - 1, am))
          val right = memo.getOrElseUpdate((i, am - coins(i)), dp(i, am - coins(i)))
          left min right + 1
      }
    }

    val res = dp(coins.length - 1, amount)
    if (res == Double.PositiveInfinity) -1 else res.toInt
  }

  def coinChange(coins: Array[Int], amount: Int): Int = {
    var memo: Array[Double] = 0.toDouble +: Array.fill(amount)(Double.PositiveInfinity)

    for (i <- coins.indices) {
      val newMemo = 0.toDouble +: Array.fill(amount)(Double.PositiveInfinity)
      for (am <- 1 to amount) {
        val x = if (am - coins(i) < 0) Double.PositiveInfinity else newMemo(am - coins(i)) + 1
        newMemo(am) = memo(am) min x
      }
      memo = newMemo
    }

    val res = memo(amount)
    if (res == Double.PositiveInfinity) -1 else res.toInt
  }

  def change(amount: Int, coins: Array[Int]): Int = {
    val arr: Array[Int] = 1 +: Array.fill(amount)(0)
    for (coin <- coins; i <- coin to amount) arr(i) += arr(i - coin)
    arr.last
  }

  def changeFP(amount: Int, coins: Array[Int]): Int = {
    val arr = 1 +: Array.fill(amount)(0)
    coins.foreach {
      coin =>
        arr.zipWithIndex.foreach {
          case (elem, i) =>
            arr(i) = elem + arr.applyOrElse(i - coin, (_: Int) => 0)
        }
    }
    arr.last
  }

  def wordBreakRecursive(s: String, wordDict: List[String]): Boolean = {
    val memo: Array[Array[Option[Boolean]]] = Array.fill(s.length)(Array.fill(s.length)(None))

    def dp(start_i: Int, end_i: Int): Boolean = {
      memo(end_i)(start_i) match {
        case Some(_) => memo(end_i)(start_i).get
        case None =>
          var flag = false
          var k = end_i
          while (!flag & k >= 0) {
            val word = s.slice(k, end_i + 1)
            val is_word = wordDict.contains(word)
            flag = is_word && (if (k != 0) dp(0, k - 1) else true)
            memo(end_i)(k) = Some(flag)
            k -= 1
          }
          flag
      }
    }

    dp(0, s.length - 1)
  }

  def lengthOfLISBruteForce(nums: Array[Int]): Int = {
    /** Given an integer array nums,
     * return the length of the longest strictly increasing subsequence.
     *
     * A subsequence is a sequence that can be derived from an array by deleting some or no elements without changing the order of the remaining elements.
     * For example, [3,6,2,7] is a subsequence of the array [0,3,1,6,2,2,7]. */
    var result = 1
    val arrLIS = Array.fill(nums.length)(0)
    arrLIS(0) = 1

    for (i <- 1 until nums.length) {
      arrLIS(i) = 1
      for (j <- i - 1 to 0 by -1) {
        if (nums(i) > nums(j)) {
          arrLIS(i) = arrLIS(i) max (arrLIS(j) + 1)
        }
      }
      result = result max arrLIS(i)
    }
    result
  }

  def numWaysRecursive(n: Int, k: Int): Int = {
    /**
     * You are painting a fence of n posts with k different colors. You must paint the posts following these rules:
     *
     * Every post must be painted exactly one color.
     * There cannot be three or more consecutive posts with the same color.
     * Given the two integers n and k, return the number of ways you can paint the fence.
     */

    val memo: mutable.HashMap[(Int, Int, Int), Int] = mutable.HashMap()

    def dp(thisWays: Int, i: Int, conseq: Int): Int = {
      (thisWays, i, conseq) match {
        case (_, _, conseq) if conseq == 3 => 0
        case (thisWays, i, _) if i == n => thisWays
        case _ => thisWays * (
          memo.getOrElseUpdate((1, i + 1, conseq + 1), dp(1, i + 1, conseq + 1)) +
            memo.getOrElseUpdate((k - 1, i + 1, 1), dp(k - 1, i + 1, 1)))
      }
    }

    dp(k, 1, 1)
  }

  def uniqueLetterStringFP(s: String): Int = {
    /**
     * Let's define a function countUniqueChars(s) that returns the number of unique characters on s.
     *
     * For example, calling countUniqueChars(s) if s = "LEETCODE" then "L", "T", "C", "O", "D" are the unique characters since they appear only once in s, therefore countUniqueChars(s) = 5.
     * Given a string s, return the sum of countUniqueChars(t) where t is a substring of s.
     *
     * Notice that some substrings can be repeated so in this case you have to count the repeated ones too.
     * */

    def countUniqueChars(s: String): Int =
      s
        .groupBy(identity)
        .mapValues(_.length)
        .count({ case (_, occurence) => occurence == 1 })

    (for (x <- 0 until s.length; y <- x until s.length) yield (x, y))
      .foldLeft(0) { (x, indices) => x + countUniqueChars(s.slice(indices._1, indices._2 + 1)) }

  }

  def numDecodings(s: String): Int = {

    val hm = new mutable.HashMap[Int, Int]()

    def rec(i: Int): Int = {
      val oneDigit = s(i).asDigit
      if (i == 0) oneDigit.signum
      else {
        val twoDigit = s"${s(i - 1)}${s(i)}".toInt
        (oneDigit, twoDigit) match {
          case (_, t) if i == 1 => (if (t >= 10 && t < 27) 1 else 0) + ((t % 10).signum min (t / 10).signum)
          case (_, t) if t % 10 == 0 && t != 20 && t != 10 => 0
          case (_, t) if t % 10 == 0 => hm.getOrElseUpdate(i - 2, rec(i - 2))
          case (_, t) if t > 26 || t < 10 => hm.getOrElseUpdate(i - 1, rec(i - 1))
          case _ => hm.getOrElseUpdate(i - 1, rec(i - 1)) + hm.getOrElseUpdate(i - 2, rec(i - 2))
        }
      }
    }

    rec(s.length - 1)
  }

}
