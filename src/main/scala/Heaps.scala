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

}
