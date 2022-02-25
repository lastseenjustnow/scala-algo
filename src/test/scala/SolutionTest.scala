import org.scalatest.FunSuite
import Solution.{climbStairs, climbStairsRecursive}


class SolutionTest extends FunSuite {
  test("Climb stairs") {
    assert(climbStairs(-1) == 0)
    assert(climbStairs(0) == 0)
    assert(climbStairs(1) == 1)
    assert(climbStairs(2) == 2)
    assert(climbStairs(3) == 3)
    assert(climbStairs(4) == 5)
    assert(climbStairs(5) == 8)
  }

  test("Climb stairs recursive") {
    assert(climbStairsRecursive(1) == 1)
    assert(climbStairsRecursive(2) == 2)
    assert(climbStairsRecursive(3) == 3)
    assert(climbStairsRecursive(4) == 5)
    assert(climbStairsRecursive(5) == 8)
  }

}