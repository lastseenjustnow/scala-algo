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

}
