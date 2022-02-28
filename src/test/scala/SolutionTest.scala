import org.scalatest.FunSuite
import Solution._


class SolutionTest extends FunSuite {
  test("Climb stairs") {
    val conditions = Array((-1, 0), (0, 0), (1, 1), (2, 2), (3, 3), (4, 5), (5, 8))

    for (cond <- conditions) {
      assert(climbStairs(cond._1) == cond._2)
      assert(climbStairsRecursive(cond._1) == cond._2)
    }
  }

  test("House Robber") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(1, 2, 3, 1), 4),
      (Array(2, 7, 9, 3, 1), 12),
      (Array(114, 117, 207, 117, 235, 82, 90, 67, 143, 146, 53, 108, 200, 91, 80, 223, 58, 170, 110, 236, 81, 90, 222, 160, 165, 195, 187, 199, 114, 235, 197, 187, 69, 129, 64, 214, 228, 78, 188, 67, 205, 94, 205, 169, 241, 202, 144, 240), 4173),
      (Array(0), 0),
      (Array(7), 7)
    )

    for (cond <- conditions) {
      assert(rob(cond._1) == cond._2)
      assert(robRecursive(cond._1) == cond._2)
    }
  }

  test("Min Cost Climbing Stairs") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(10, 15, 20), 15),
      (Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1), 6)
    )

    for (cond <- conditions) {
      assert(minCostClimbingStairs(cond._1) == cond._2)
      assert(minCostClimbingStairsRecursive(cond._1) == cond._2)
    }
  }

}