import scala.collection.mutable

object Solution {
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
    val sums = Array.fill(nums.max) {
      0
    }
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

}