import scala.collection.mutable

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

}
