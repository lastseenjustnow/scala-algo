package contests

import scala.collection.mutable


object BiweeklyContest108 {
  def alternatingSubarray(nums: Array[Int]): Int = {
    var subStart = 0
    var res = -1
    for (i <- 1 until nums.length) {
      if (nums(i) - nums(i - 1) == Math.pow(-1, i - subStart + 1)) {
        res = res max (i - subStart + 1)
      } else if (nums(i) == nums(i - 1) + 1) {
        subStart = i - 1
        res = res max (i - subStart + 1)
      }
      else subStart = i
    }
    res
  }

  def relocateMarbles(nums: Array[Int], moveFrom: Array[Int], moveTo: Array[Int]): List[Int] = {
    val map: mutable.SortedMap[Long, Long] = mutable.SortedMap()
    nums.foreach {
      num => {
        val thisC = map.getOrElseUpdate(num, 0L)
        map(num) = thisC + 1
      }
    }

    moveFrom.zip(moveTo).foreach {
      pair => {
        val thisCount = map(pair._1)
        val newCount = map.getOrElseUpdate(pair._2, 0L)
        map(pair._2) = newCount + thisCount
        if (pair._1 != pair._2) map.remove(pair._1)
      }
    }

    map.keys.toList.map( x => x.toInt)
  }


}
