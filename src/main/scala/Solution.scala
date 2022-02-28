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
     * Bottom up implementation
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

}