import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

}
