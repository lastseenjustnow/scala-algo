import scala.collection.mutable

object Heaps {
  def lastStoneWeight(stones: Array[Int]): Int = {
    val maxHeap = new mutable.PriorityQueue[Int]
    stones.foreach(elem => maxHeap.enqueue(elem))

    while (maxHeap.length > 1) {
      val firstStone = maxHeap.dequeue()
      val secondStone = maxHeap.dequeue()
      if (firstStone != secondStone) {
        maxHeap.enqueue(firstStone - secondStone)
      }
    }

    if (maxHeap.nonEmpty) maxHeap.dequeue() else 0
  }

  class KthLargest(_k: Int, _nums: Array[Int]) {

    val minHeap = new mutable.PriorityQueue[Int]()(Ordering.Int.reverse)
    minHeap ++= _nums
    while (minHeap.size > _k) {
      minHeap.dequeue()
    }

    def add(`val`: Int): Int = {
      minHeap.enqueue(`val`)
      while (minHeap.size > _k) {
        minHeap.dequeue()
      }
      minHeap.head
    }
  }

  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    val freqs = nums.groupBy(identity).mapValues(_.length).toSet
    val pq = mutable.PriorityQueue[(Int, Int)]()(Ordering.by((_: (Int, Int))._2).reverse)
    for (elem <- freqs) pq.enqueue(elem)
    while (pq.size > k) pq.dequeue()
    pq.map(_._1).toArray

  }

  def minProductSum(nums1: Array[Int], nums2: Array[Int]): Int = {
    val maxHeap = new mutable.PriorityQueue[Int]()
    val minHeap = new mutable.PriorityQueue[Int]()(Ordering.Int.reverse)

    for (i <- nums1.indices) {
      maxHeap.enqueue(nums1(i))
      minHeap.enqueue(nums2(i))
    }

    var res = 0
    for (_ <- nums1.indices) {
      res += minHeap.dequeue() * maxHeap.dequeue()
    }

    res

  }

  def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] = {
    val freqs = mat.zipWithIndex.map(x => (x._1.lastIndexOf(1) + 1, x._2))
    val order: Ordering[(Int, Int)] = Ordering.Tuple2(Ordering.Int.reverse, Ordering.Int.reverse)
    val maxHeap = new mutable.PriorityQueue[(Int, Int)]()(order)
    for (freq <- freqs) maxHeap.enqueue(freq)
    val res = Array.fill(k)(0)
    for (i <- res.indices) res(i) = maxHeap.dequeue()._2
    res
  }

  def kthSmallestNaive(matrix: Array[Array[Int]], k: Int): Int = {
    /** Time complexity: O(n ** 2 log n) */
    val maxHeap = new mutable.PriorityQueue[Int]()
    matrix.flatten.foreach(x => maxHeap.enqueue(x))
    while (maxHeap.size > k) maxHeap.dequeue()
    maxHeap.head
  }

  def kthSmallestHeap(matrix: Array[Array[Int]], k: Int): Int = {
    val n = matrix.length
    val minHeap = new mutable.PriorityQueue[(Int, Int, Int)]()(Ordering.Tuple3(Ordering.Int.reverse, Ordering.Int.reverse, Ordering.Int.reverse))
    for (i <- 0 until (n min k)) minHeap.enqueue((matrix(i)(0), i, 0))

    for (_ <- 1 until k) {
      val minVal = minHeap.dequeue()
      if (minVal._3 != n - 1) minHeap.enqueue((matrix(minVal._2)(minVal._3 + 1), minVal._2, minVal._3 + 1))
    }
    minHeap.head._1
  }

  def minMeetingRoomsFP(intervals: Array[Array[Int]]): Int = {
    intervals
      .flatMap(interval => Array((interval(0), 0), (interval(1), 1)))
      .sorted(Ordering.Tuple2(Ordering.Int, Ordering.Int.reverse))
      .foldLeft((0, 0)) {
        case (res, elem) =>
          val openedMeetings = res._1 + (if (elem._2 == 1) -1 else 1)
          val maxMeetings = res._2 max openedMeetings
          (openedMeetings, maxMeetings)
      }._2
  }

  def minMeetingRooms(intervals: Array[Array[Int]]): Int = {
    val sortedIntervals = intervals
      .map(x => (x(0), x(1)))
      .sorted(Ordering.Tuple2(Ordering.Int, Ordering.Int.reverse))

    val minHeap = new mutable.PriorityQueue[Int]()(Ordering.Int.reverse)
    var maxHeapSize = 0

    for (elem <- sortedIntervals) {
      if (minHeap.isEmpty || elem._1 < minHeap.head) {
        minHeap.enqueue(elem._2)
      } else {
        minHeap.dequeue()
        minHeap.enqueue(elem._2)
      }
      maxHeapSize = maxHeapSize max minHeap.size
    }
    maxHeapSize
  }

  def kClosest(points: Array[Array[Int]], k: Int): Array[Array[Int]] = {
    def euclidDistance(x: Int, y: Int): Double = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))

    val ordering = Ordering.by[(Double, Array[Int]), Double](_._1).reverse
    val minHeap: mutable.PriorityQueue[(Double, Array[Int])] = mutable.PriorityQueue[(Double, Array[Int])]()(ordering)
    for (point <- points) minHeap.enqueue((euclidDistance(point(0), point(1)), point))
    (for (_ <- 1 to k) yield minHeap.dequeue()._2).toArray
  }

}
