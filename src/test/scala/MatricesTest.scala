import org.scalatest.{FunSuite, color}
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

  test("Spiral Matrix II") {
    val conditions: Array[(Int, Array[Array[Int]])] =
      Array(
        (1, Array(Array(1))),
        (2, Array(Array(1, 2), Array(4, 3))),
        (3, Array(Array(1, 2, 3), Array(8, 9, 4), Array(7, 6, 5))),
        (4, Array(Array(1, 2, 3, 4), Array(12, 13, 14, 5), Array(11, 16, 15, 6), Array(10, 9, 8, 7))),
        (5, Array(Array(1, 2, 3, 4, 5), Array(16, 17, 18, 19, 6), Array(15, 24, 25, 20, 7), Array(14, 23, 22, 21, 8), Array(13, 12, 11, 10, 9)))
      )

    for (cond <- conditions) {
      assert(generateMatrix(cond._1).toSeq.map(_.toSeq) == cond._2.toSeq.map(_.toSeq), f"wrong for n: ${cond._1.toString}")
    }
  }

  test("Count Negative Numbers in a Sorted Matrix") {
    val conditions: Array[(Array[Array[Int]], Int)] =
      Array(
        (Array(Array(4, 3, 2, -1), Array(3, 2, 1, -1), Array(1, 1, -1, -2), Array(-1, -1, -2, -3)), 8),
        (Array(Array(3, 2), Array(1, 0)), 0),
      )

    for (cond <- conditions) {
      assert(countNegatives(cond._1) == cond._2, f"wrong for n: ${cond._1.mkString(",")}")
    }
  }

  test("Search a 2D Matrix") {
    val conditions: Array[(Array[Array[Int]], Int, Boolean)] =
      Array(
        (Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)), 3, true),
        (Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)), 13, false),
        (Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)), 20, true),
        (Array(Array(6)), 6, true),
        (Array(Array(1, 1)), 2, false),
      )

    for (cond <- conditions) {
      assert(searchMatrix(cond._1, cond._2) == cond._3)
    }
  }

  test("Count Unguarded Cells in the Grid") {
    val conditions: Array[(Int, Int, Array[Array[Int]], Array[Array[Int]], Int)] =
      Array(
        (4, 6, Array(Array(0, 0), Array(1, 1), Array(2, 3)), Array(Array(0, 1), Array(2, 2), Array(1, 4)), 7),
        (3, 3, Array(Array(1, 1)), Array(Array(0, 1), Array(1, 0), Array(2, 1), Array(1, 2)), 4),
      )

    for (cond <- conditions) {
      assert(countUnguarded(cond._1, cond._2, cond._3, cond._4) == cond._5)
    }
  }

}
