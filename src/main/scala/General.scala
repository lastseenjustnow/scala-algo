import scala.collection.mutable
import scala.math.{abs, max}

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

}
