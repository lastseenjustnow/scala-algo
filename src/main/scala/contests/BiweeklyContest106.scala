package contests

import scala.collection.mutable

object BiweeklyContest106 {
  def problem1(n: Int): Boolean = {
    val str: List[Char] = (n.toString + (n * 2).toString + (n * 3).toString).toList
    str.length == 9 && str.distinct.length == 9 && !str.contains('0')
  }

  def longestSemiRepetitiveSubstring(s: String): Int = {

    var curLen = 1
    var semiRep = -1
    var maxLen = 1

    for (i <- 1 until s.length) {
      if (s(i) == s(i - 1)) {
        if (semiRep != -1) {
          curLen = i - semiRep
        }
        semiRep = i
      }
      curLen += 1
      maxLen = maxLen max curLen
    }
    maxLen

  }

  def sumDistance(nums: Array[Int], s: String, d: Int): Int = {

    def performCommand(pos: Int, c: Char): Int = if (c == 'R') pos + 1 else pos - 1

    for (_ <- 1 to d; robotNumber <- nums.indices) {
      nums(robotNumber) = performCommand(nums(robotNumber), s(robotNumber))
    }

    val numsSortedWithIndex = nums.sorted.zipWithIndex

    var cumSum = 0
    var diffSum = 0

    for (t <- numsSortedWithIndex) {
      diffSum += t._1 * t._2 - cumSum
      cumSum += t._1
    }

    (diffSum % (Math.pow(10, 9) + 7)).toInt
  }

}
