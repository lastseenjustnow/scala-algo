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

}
