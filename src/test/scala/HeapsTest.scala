import org.scalatest.FunSuite
import Heaps._

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
}
