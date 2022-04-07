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
}
