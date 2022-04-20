package datastructure

import scala.collection.mutable

class MedianFinder() {
  private val minHeap: mutable.PriorityQueue[Int] = new mutable.PriorityQueue[Int]()(Ordering.Int.reverse)
  private val maxHeap: mutable.PriorityQueue[Int] = new mutable.PriorityQueue[Int]()

  def addNum(num: Int): Unit = {
    val median = this.findMedian()
    if (num > median) minHeap.enqueue(num) else maxHeap.enqueue(num)
    if (minHeap.size > maxHeap.size) maxHeap.enqueue(minHeap.dequeue())
    if (minHeap.size < maxHeap.size - 1) minHeap.enqueue(maxHeap.dequeue())
  }

  def findMedian(): Double = {
    (maxHeap.size, minHeap.size) match {
      case (0, _) => Int.MaxValue
      case (m, n) if (m + n) % 2 == 1 => maxHeap.head
      case _ => (minHeap.head + maxHeap.head).toDouble / 2
    }
  }

}
