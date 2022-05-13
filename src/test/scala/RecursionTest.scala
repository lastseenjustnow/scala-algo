import Recursion._
import org.scalatest.FunSuite


class RecursionTest extends FunSuite {
  test("N-Queens II") {
    val conditions: Array[(Int, Int)] = Array(
      (4, 2),
      (1, 1),
      (8, 92),
      (9, 352),
    )

    for (cond <- conditions) {
      assert(totalNQueens(cond._1) == cond._2)
    }
  }

}
