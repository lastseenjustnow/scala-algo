package contests

import contests.BiweeklyContest108._
import org.scalatest.FunSuite

class BiweeklyContest108Test extends FunSuite {
  test("Longest Alternating Subarray") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(2, 3, 4, 3, 4), 4),
      (Array(4, 5, 6), 2),
      (Array(4, 3, 2), -1),
      (Array(0, 1, 0, 1, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1), 13),
    )

    for (cond <- conditions) {
      assert(alternatingSubarray(cond._1) == cond._2)
    }
  }

  test("Relocate Marbles") {
    val conditions: Array[(Array[Int], Array[Int], Array[Int], List[Int])] = Array(
      (Array(1, 6, 7, 8), Array(1, 7, 2), Array(2, 9, 5), List(5, 6, 8, 9)),
      (Array(1, 1, 3, 3), Array(1, 3), Array(2, 2), List(2)),
      (Array(3, 4), Array(4, 3, 1, 2, 2, 3, 2, 4, 1), Array(3, 1, 2, 2, 3, 2, 4, 1, 1), List(1))
    )

    for (cond <- conditions) {
      assert(relocateMarbles(cond._1, cond._2, cond._3) == cond._4)
    }
  }
}
