import org.scalatest.FunSuite
import org.scalatest.Matchers._
import Heaps.{KthLargest, _}
import datastructure.MedianFinder

class HeapsTest extends FunSuite {

  test("Last Stone Weight") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(2, 7, 4, 1, 8, 1), 1),
        (Array(1), 1)
      )


    for (cond <- conditions) {
      assert(lastStoneWeight(cond._1) == cond._2)
    }
  }

  test("Kth Largest Element in a Stream") {
    val conditions: Array[(KthLargest, Array[Int], Array[Int])] = Array(
      (new KthLargest(3, Array(4, 5, 8, 2)), Array(3, 5, 10, 9, 4), Array(4, 5, 5, 8, 8)),
    )


    for (cond <- conditions) {
      for (i <- cond._2.indices) {
        assert(cond._1.add(cond._2(i)) == cond._3(i))
      }
    }
  }

  test("Top K Frequent Elements") {
    val conditions: Array[(Array[Int], Int, Array[Int])] = Array(
      (Array(1, 1, 1, 2, 2, 3), 2, Array(1, 2)),
      (Array(1), 1, Array(1)),
      (Array(1, 0), 2, Array(0, 1)),
      (Array(1, 5, 3, 5, 3, 2, 1, 2, 2, 1, 2, 3, 4, 5, 5, 3, 3, 3, 0), 3, Array(2, 5, 3))
    )


    for (cond <- conditions) {
      topKFrequent(cond._1, cond._2) should contain allElementsOf cond._3
    }
  }


  test("Minimize Product Sum of Two Arrays") {
    val conditions: Array[(Array[Int], Array[Int], Int)] = Array(
      (Array(5, 3, 4, 2), Array(4, 2, 2, 5), 40),
      (Array(2, 1, 4, 5, 7), Array(3, 2, 4, 8, 6), 65)

    )


    for (cond <- conditions) {
      assert(minProductSum(cond._1, cond._2) == cond._3)
    }
  }

  test("The K Weakest Rows in a Matrix") {
    val conditions: Array[(Array[Array[Int]], Int, Array[Int])] = Array(
      (
        Array(Array(1, 1, 0, 0, 0),
          Array(1, 1, 1, 1, 0),
          Array(1, 0, 0, 0, 0),
          Array(1, 1, 0, 0, 0),
          Array(1, 1, 1, 1, 1)), 3, Array(2, 0, 3)),

      (Array(Array(1, 0, 0, 0),
        Array(1, 1, 1, 1),
        Array(1, 0, 0, 0),
        Array(1, 0, 0, 0)), 2, Array(0, 2))
    )


    for (cond <- conditions) {
      assert(kWeakestRows1(cond._1, cond._2) sameElements cond._3)
      assert(kWeakestRows2(cond._1, cond._2).toList == cond._3.toList)
    }
  }

  test("Kth Smallest Element in a Sorted Matrix") {
    val conditions: Array[(Array[Array[Int]], Int, Int)] = Array(
      (
        Array(Array(1, 5, 9),
          Array(10, 11, 13),
          Array(12, 13, 15)), 8, 13),
      (Array(Array(-5)), 1, -5),
      (Array(Array(1, 2), Array(3, 4)), 2, 2)
    )

    for (cond <- conditions) {
      assert(kthSmallestNaive(cond._1, cond._2) == cond._3)
      assert(kthSmallestHeap(cond._1, cond._2) == cond._3)
    }
  }

  test("Meeting Rooms II") {
    val conditions: Array[(Array[Array[Int]], Int)] = Array(
      (Array(Array(0, 30), Array(5, 10), Array(15, 20)), 2),
      (Array(Array(7, 10), Array(2, 4)), 1),
      (Array(Array(0, 30), Array(5, 10), Array(15, 20), Array(25, 35), Array(7, 17)), 3),
      (Array(Array(0, 30), Array(5, 10), Array(15, 20), Array(7, 17)), 3),
      (Array(Array(13, 15), Array(1, 13)), 1),
      (Array(Array(1, 5), Array(8, 9), Array(8, 9)), 2),
      (Array(Array(1, 8), Array(6, 20), Array(9, 16), Array(13, 17)), 3),
    )

    for (cond <- conditions) {
      assert(minMeetingRoomsFP(cond._1) == cond._2)
      assert(minMeetingRooms(cond._1) == cond._2)
    }
  }

  test("K Closest Points to Origin") {
    val conditions: Array[(Array[Array[Int]], Int, Array[Array[Int]])] = Array(
      (Array(Array(1, 3), Array(-2, 2)), 1, Array(Array(-2, 2))),
      (Array(Array(3, 3), Array(5, -1), Array(-2, 4)), 2, Array(Array(3, 3), Array(-2, 4))),
    )

    for (cond <- conditions) {
      assert(kClosest(cond._1, cond._2).map(_.toList) sameElements cond._3.map(_.toList))
    }
  }

  test("Minimum Cost to Connect Sticks") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(2, 4, 3), 14),
      (Array(1, 8, 3, 5), 30),
      (Array(5), 0),
    )

    for (cond <- conditions) {
      assert(connectSticks(cond._1) == cond._2)
    }
  }

  test("Furthest Building You Can Reach") {
    val conditions: Array[(Array[Int], Int, Int, Int)] = Array(
      (Array(4, 2, 7, 6, 9, 14, 12), 5, 1, 4),
      (Array(4, 12, 2, 7, 3, 18, 20, 3, 19), 10, 2, 7),
      (Array(14, 3, 19, 3), 17, 0, 3),
      (Array(7, 5, 13), 0, 0, 1),
    )

    for (cond <- conditions) {
      assert(furthestBuilding(cond._1, cond._2, cond._3) == cond._4)
    }
  }

  test("Find Median from Data Stream") {
    val medFinder = new MedianFinder()
    medFinder.addNum(1)
    medFinder.addNum(2)
    assert(medFinder.findMedian() == 1.5)
    medFinder.addNum(3)
    assert(medFinder.findMedian() == 2)
    medFinder.addNum(7)
    medFinder.addNum(9)
    medFinder.addNum(16)
    medFinder.addNum(15)
    assert(medFinder.findMedian() == 7)
    medFinder.addNum(-100)
    medFinder.addNum(-101)
    medFinder.addNum(-102)
    medFinder.addNum(-103)
    assert(medFinder.findMedian() == 2)
  }

  test("Min Cost to Connect All Points") {
    val conditions: Array[(Array[Array[Int]], Int)] = Array(
      (Array(Array(0, 0), Array(2, 2), Array(3, 10), Array(5, 2), Array(7, 0)), 20),
      (Array(Array(3, 12), Array(-2, 5), Array(-4, 1)), 18),
      (Array(Array(0, 0)), 0),
      (Array(Array(2, -3), Array(-17, -8), Array(13, 8), Array(-17, -15)), 53),
      (Array(Array(0, 0), Array(1, 1), Array(1, 0), Array(-1, 1)), 4)
    )

    for (cond <- conditions) {
      assert(minCostConnectPoints(cond._1) == cond._2)
      assert(minCostConnectPointsKruskal(cond._1) == cond._2)
    }
  }

}
