import org.scalatest.FunSuite
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

}
