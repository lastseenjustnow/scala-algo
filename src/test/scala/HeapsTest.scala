import org.scalatest.FunSuite
import org.scalatest.Matchers._
import Heaps.{KthLargest, _}

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
      assert(kWeakestRows(cond._1, cond._2) sameElements cond._3)
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


}
