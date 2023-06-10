package contests

import scala.math.BigDecimal

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

    def performCommand(pos: BigDecimal, c: Char): BigDecimal = if (c == 'R') pos + d else pos - d

    val numsBig = nums.map(x => BigDecimal(x))

    for (robotNumber <- nums.indices) {
      numsBig(robotNumber) = performCommand(numsBig(robotNumber), s(robotNumber))
    }

    val numsSortedWithIndex = numsBig.sorted.zipWithIndex

    var cumSum = BigDecimal(0)
    var diffSum = BigDecimal(0)

    for (t <- numsSortedWithIndex) {
      diffSum += t._1 * t._2 - cumSum
      cumSum += t._1
    }

    (diffSum % (Math.pow(10, 9) + 7)).toInt
  }

}
