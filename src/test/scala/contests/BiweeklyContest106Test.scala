package contests

import contests.BiweeklyContest106._
import org.scalatest.{FunSuite, color}

class BiweeklyContest106Test extends FunSuite {
  test("Problem 1") {
    val conditions: Array[(Int, Boolean)] = Array(
      (192, true),
      (100, false),
      (181, false),
      (1000, false),
      (267, false),
      (783, false)
    )

    for (cond <- conditions) {
      assert(problem1(cond._1) == cond._2)
    }
  }

  test("Problem 2") {
    val conditions: Array[(String, Int)] = Array(
      ("52233", 4),
      ("5494", 4),
      ("11111111", 2),
      ("522336748", 7),
      ("5223367789", 6)
    )

    for (cond <- conditions) {
      assert(longestSemiRepetitiveSubstring(cond._1) == cond._2)
    }
  }

  test("Problem 3") {
    val conditions: Array[(Array[Int], String, Int, Int)] = Array(
      (Array(-2, 0, 2), "RLL", 3, 8),
      (Array(1, 0), "RL", 2, 5),
      (Array(1, 0, 2), "RLR", 2, 12),
      (Array(1, 4, 10, 16), "RLRR", 0, 51),
    )

    for (cond <- conditions) {
      assert(sumDistance(cond._1, cond._2, cond._3) == cond._4)
    }
  }

}
