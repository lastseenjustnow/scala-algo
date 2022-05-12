import MathProblems._
import org.scalatest.FunSuite
import org.scalatest.Matchers._


class MathProblemsTest extends FunSuite {
  test("Pow(x, n)") {
    val conditions: Array[(Double, Int, Double)] =
      Array(
        (2, 4, 16),
        (2, 10, 1024),
        (2.10000, 3, 9.26100),
        (2.00000, -2, 0.25000),
        (2.00000, 0, 1),
        (9, 15, 205891132094649D),
      )

    val Eps = 1e-7

    for (cond <- conditions) {
      myPowRecursive(cond._1, cond._2) should be(cond._3 +- Eps)
      myPowIterative(cond._1, cond._2) should be(cond._3 +- Eps)
    }
  }

  test("Count Primes") {
    val conditions: Array[(Int, Int)] =
      Array(
        (10, 4),
        (0, 0),
        (1, 0),
        (2, 0),
        (30, 10),
        (120, 30),
        (5000000, 348513)
      )

    for (cond <- conditions) {
      assert(countPrimes(cond._1) == cond._2)
    }
  }

  test("Permutations II") {
    val conditions: Array[(Array[Int], List[List[Int]])] =
      Array(
        (Array(1, 1, 2), List(List(1, 1, 2), List(1, 2, 1), List(2, 1, 1))),
        (Array(1, 2, 3), List(List(1, 2, 3), List(1, 3, 2), List(3, 2, 1), List(3, 1, 2), List(2, 3, 1), List(2, 1, 3))),
        (Array(1, 4, 6, 7), List(List(1, 4, 6, 7), List(1, 4, 7, 6), List(1, 6, 4, 7), List(1, 6, 7, 4), List(1, 7, 4, 6), List(1, 7, 6, 4), List(4, 1, 6, 7), List(4, 1, 7, 6), List(4, 6, 1, 7), List(4, 6, 7, 1), List(4, 7, 1, 6), List(4, 7, 6, 1), List(6, 1, 4, 7), List(6, 1, 7, 4), List(6, 4, 1, 7), List(6, 4, 7, 1), List(6, 7, 1, 4), List(6, 7, 4, 1), List(7, 1, 4, 6), List(7, 1, 6, 4), List(7, 4, 1, 6), List(7, 4, 6, 1), List(7, 6, 1, 4), List(7, 6, 4, 1))),
        (Array(0, 0, 0, 0, 0, 0, 0, 0), List(List(0, 0, 0, 0, 0, 0, 0, 0)))
      )

    for (cond <- conditions) {
      permuteUnique(cond._1) should contain theSameElementsAs cond._2
    }
  }

}
