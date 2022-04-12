import org.scalatest.FunSuite

import Matrices._

class MatricesTest extends FunSuite {
  test("Game of Life Naive") {
    val conditions: Array[(Array[Array[Int]], Array[Array[Int]])] =
      Array(
        (Array(Array(0, 1, 0), Array(0, 0, 1), Array(1, 1, 1), Array(0, 0, 0)), Array(Array(0, 0, 0), Array(1, 0, 1), Array(0, 1, 1), Array(0, 1, 0))),
        (Array(Array(1, 1), Array(1, 0)), Array(Array(1, 1), Array(1, 1)))
      )

    for (cond <- conditions) {
      gameOfLifeNaive(cond._1)
      cond._1.indices.foreach(i => assert(cond._1(i) sameElements cond._2(i)))
    }
  }

  test("Game of Life - constant space") {
    val conditions: Array[(Array[Array[Int]], Array[Array[Int]])] =
      Array(
        (Array(Array(0, 1, 0), Array(0, 0, 1), Array(1, 1, 1), Array(0, 0, 0)), Array(Array(0, 0, 0), Array(1, 0, 1), Array(0, 1, 1), Array(0, 1, 0))),
        (Array(Array(1, 1), Array(1, 0)), Array(Array(1, 1), Array(1, 1)))
      )

    for (cond <- conditions) {
      gameOfLife(cond._1)
      cond._1.indices.foreach(i => assert(cond._1(i) sameElements cond._2(i)))
    }
  }

}
