import SortingAndSearching._
import org.scalatest.FunSuite

class SortingAndSearchingTest extends FunSuite {
  test("Pow(x, n)") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 2, 1, 5, 6, 4), 2, 5),
        (Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4, 4),

      )

    val Eps = 1e-7

    for (cond <- conditions) {
      assert(findKthLargest(cond._1, cond._2) == cond._3)
    }
  }
}
