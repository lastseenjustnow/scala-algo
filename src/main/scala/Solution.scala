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
}