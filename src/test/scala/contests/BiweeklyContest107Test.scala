package contests

import contests.BiweeklyContest107._
import org.scalatest.FunSuite

class BiweeklyContest107Test extends FunSuite {
  test("Problem 1") {
    val conditions: Array[(Array[String], Int)] = Array(
      (Array("cd", "ac", "dc", "ca", "zz"), 2),
      (Array("ab", "ba", "cc"), 1),
      (Array("aa", "ab"), 0),
      (Array(), 0)
    )

    for (cond <- conditions) {
      assert(maximumNumberOfStringPairs(cond._1) == cond._2)
    }
  }
}
