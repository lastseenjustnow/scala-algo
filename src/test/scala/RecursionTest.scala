import Recursion._
import org.scalatest.FunSuite


class RecursionTest extends FunSuite {

  test("N-Queens") {
    val conditions: Array[(Int, List[List[String]])] = Array(
      (4, List(List(".Q..", "...Q", "Q...", "..Q."), List("..Q.", "Q...", "...Q", ".Q.."))),
      (1, List(List("Q")))
    )

    for (cond <- conditions) {
      assert(solveNQueens(cond._1) == cond._2)
    }
  }


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
