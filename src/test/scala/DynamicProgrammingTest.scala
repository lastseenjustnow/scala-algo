import org.scalatest.FunSuite
import DynamicProgramming._


class DynamicProgrammingTest extends FunSuite {
  test("Climb stairs") {
    val conditions = Array((1, 1), (2, 2), (3, 3), (4, 5), (5, 8), (45, 1836311903))

    for (cond <- conditions) {
      assert(climbStairs(cond._1) == cond._2)
      assert(climbStairsRecursive(cond._1) == cond._2)
      assert(climbStairsFP(cond._1) == cond._2)
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
      assert(robFP(cond._1) == cond._2)
    }
  }

  test("House Robber II") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(2, 3, 2), 3),
      (Array(1, 2, 3, 1), 4),
      (Array(2, 3, 3), 3),
      (Array(5, 4, 3), 5),
      (Array(3, 4, 3), 4),
      (Array(3, 4, 3, 1), 6),
      (Array(5, 4, 3, 1), 8),
      (Array(5, 26, 3, 19, 1), 45),
      (Array(5, 4, 3, 1, 8), 12),
      (Array(5, 4, 3, 8), 12),
      (Array(0), 0),
      (Array(7), 7),
      (Array(1, 2), 2),
      (Array(1, 2, 1, 1), 3)
    )

    for (cond <- conditions) {
      assert(rob2(cond._1) == cond._2)
      assert(rob2FP(cond._1) == cond._2)
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
      assert(minCostClimbingStairsFP(cond._1) == cond._2)
    }
  }

  test("Fibonacci Number") {
    val conditions = Array((-1, 0), (0, 0), (2, 1), (3, 2), (4, 3), (20, 6765))

    for (cond <- conditions) {
      assert(fib(cond._1) == cond._2)
      assert(fibFP(cond._1) == cond._2)
    }
  }

  test("N-th Tribonacci Number") {
    val conditions = Array((0, 0), (1, 1), (2, 1), (3, 2), (4, 4), (25, 1389537))

    for (cond <- conditions) {
      assert(tribonacci(cond._1) == cond._2)
      assert(tribonacciRecursive(cond._1) == cond._2)
      assert(tribonacciFP(cond._1) == cond._2)
    }
  }

  test("Delete and Earn") {
    val conditions = Array(
      (Array(3, 4, 2), 6),
      (Array(2, 2, 3, 3, 3, 4), 9),
      (Array(14, 12, 13, 17, 22, 6, 14, 41, 50, 40, 26), 202),
      (Array(1, 8, 5, 9, 6, 9, 4, 1, 7, 3, 3, 6, 3, 3, 8, 2, 6, 3, 2, 2, 1, 2, 9, 8, 7, 1, 1, 10, 6, 7, 3, 9, 6, 10, 5, 4, 10, 1, 6, 7, 4, 7, 4, 1, 9, 5, 1, 5, 7, 5), 138)
    )

    for (cond <- conditions) {
      assert(deleteAndEarn(cond._1) == cond._2)
    }
  }

  test("Maximum Score from Performing Multiplication Operations") {
    val conditions = Array(
      (Array(1, 2, 3), Array(3, 2, 1), 14),
      (Array(-5, -3, -3, -2, 7, 1), Array(-10, -5, 3, 4, 6), 102),
      (Array(-947, 897, 328, -467, 14, -918, -858, -701, -518, -997, 22, 259, -4, 968, 947, 582, -449, 895, -121, -403, 633, 490, 64, 543, -396, -997, 841, -398, 247, 297, -147, -708, 804, -199, -765, -547, -599, 406, -223, -11, 663, 746, -365, -859, 256, -25, 919, -334, 490, -511, 865, -139, -968, 961, -793, 451, 317, 645, -294, 240, -312, -822, 961, -572, 309, 579, 161, 780, 525, -622, -511, 423, 946, -28, -199, 822, -123, -316, -913, 113, -458, -428, -414, 49, 922, 722, -854, 323, -219, 581, 302, 124, 164, 31, 727, 186, 308, -936, -937, -862, 939, 213, 966, -74, -76, -1, 521, 777, -966, 454, -199, 526, -895, 447, -749, -518, -639, 849, -771, 979, -760, -763, -601, -201, 40, -911, 207, 890, -942, -352, 700, 267, 830, -396, 536, 877, -896, -687, 262, -60, -373, -373, 526), Array(864, 849, 586, 769, 309, -413, 318, 652, 883, -690, 796, 251, -648, 433, 1000, -969, 422, 194, -785, -242, -118, 69, 187, -925, -418, -556, 88, -399, -619, -383, -188, 206, -793, -9, 738, -587, 878, 360, 640, 318, -399, -366, 365, -291, -75, -451, -674, -199, 177, 862, 1, 11, 390, -52, -101, 127, -354, -717, -717, 180, 655, 817, -898, 28, -641, -35, -563, -737, 283, -223, -322, -59, 955, 172, 230, 512, -205, -180, 899, 169, -663, -253, 270, 651, 168, 417, 613, -443, 980, -189, 417, 372, -891, -272, 993, -877, 906, 680, -630, -328, -873, -811, 78, -667, -2, 190, -773, 878, 529, 620, -951, -687, 314, -989, -48, -601, -950, 31, -789, -663, -480, 750, -872, -358, 529, -426, -111, 517, 750, -536, -673, 370), 32383191)
    )

    for (cond <- conditions) {
      assert(maximumScoreRecursive(cond._1, cond._2) == cond._3)
      assert(maximumScore(cond._1, cond._2) == cond._3)
    }
  }

  test("Longest Common Subsequence") {
    val conditions = Array(
      ("abcde", "ace", 3),
      ("abc", "abc", 3),
      ("abc", "def", 0),
      ("bsbininm", "jmjkbkjkv", 1),
      ("bl", "yby", 1)
    )

    for (cond <- conditions) {
      assert(longestCommonSubsequenceRecursive(cond._1, cond._2) == cond._3)
      assert(longestCommonSubsequence(cond._1, cond._2) == cond._3)
    }
  }

  test("Maximal Square") {
    val conditions: Array[(Array[Array[Char]], Int)] = Array(
      (Array(
        "10100",
        "10111",
        "11111",
        "10010").map(x => x.toCharArray), 4),

      (Array(
        "00000",
        "10111",
        "11111",
        "10010").map(x => x.toCharArray), 4),

      (Array(
        "00000",
        "10000",
        "00000",
        "00000").map(x => x.toCharArray), 1),

      (Array(
        "00000",
        "00000",
        "00100",
        "00000").map(x => x.toCharArray), 1),

      (Array(
        "0000010",
        "1011101",
        "1111111",
        "1010111").map(x => x.toCharArray), 4),

      (Array(
        "00000",
        "10111",
        "11111",
        "10111").map(x => x.toCharArray), 9),

      (Array(
        "0000010",
        "1011101",
        "1111111",
        "1011111").map(x => x.toCharArray), 9),

      (Array(
        "00000100",
        "10111010",
        "11111110",
        "10111110",
        "10111110",
        "10111110"
      ).map(x => x.toCharArray), 16),

      (Array(
        "01",
        "10").map(x => x.toCharArray), 1),

      (Array("10").map(x => x.toCharArray), 1),
      (Array("0", "1").map(x => x.toCharArray), 1)


    )

    for (cond <- conditions) {
      assert(maximalSquare(cond._1) == cond._2)
      assert(maximalSquareRecursive(cond._1) == cond._2)
    }
  }

  test("Jump Game") {
    val conditions: Array[(Array[Int], Boolean)] = Array(
      (Array(2, 3, 1, 1, 4), true),
      (Array(3, 2, 1, 0, 4), false),
      (Array(1), true),
      (Array(0), true),
      (Array(1, 0), true),
      (Array(0, 1), false),
      (Array(2, 0), true),
      (Array(2, 0, 0), true)
    )

    for (cond <- conditions) {
      assert(canJumpRecursive(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(canJumpQueue(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(canJump(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(canJumpFP(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Jump Game II") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(2, 3, 1, 1, 4), 2),
      (Array(2, 3, 0, 1, 4), 2),
      (Array(0), 0),
      (Array(1, 0), 1),
      (Array(2, 0), 1),
      (Array(2, 0, 0), 1)
    )

    for (cond <- conditions) {
      assert(canJump2Naive(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(canJump2(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(canJump2FP(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Maximum Subarray") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(-2, 1, -3, 4, -1, 2, 1, -5, 4), 6),
      (Array(1), 1),
      (Array(5, 4, -1, 7, 8), 23),
      (Array(-7, 0, -1, 7, 8), 15),
      (Array(-7, 0, -1, -2, -4), 0),
      (Array(-5, -7, -1, -2, -4), -1),
      (Array(5, -3, 5, 5, -3, 5), 14),
      (Array(-2, 4, -5, 4, -5, 9, 4), 13),
      (Array(5, 5, -3), 10)
    )

    for (cond <- conditions) {
      assert(maxSubArray(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(maxSubArrayKadane(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Minimum Subarray") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(-2, 1, -3, 4, -1, 2, 1, -5, 4), -5),
      (Array(1), 1),
      (Array(5, 4, -1, 7, 8), -1),
      (Array(-7, 0, -1, 7, 8), -8),
      (Array(-7, 0, -1, -2, -4), -14),
      (Array(-5, -7, -1, -2, -4), -19),
      (Array(5, -3, 5, 5, -3, 5), -3),
      (Array(-2, 4, -5, 4, -5, 9, 4), -6),
      (Array(5, 5, -3), -3)
    )

    for (cond <- conditions) {
      assert(minSubArrayKadane(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Maximum Sum Circular Subarray") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(1, -2, 3, -2), 3),
      (Array(1), 1),
      (Array(5, -3, 5), 10),
      (Array(-3, -2, -3), -2),
      (Array(5, 4, -1, 7, 8), 24),
      (Array(5, 4, -1, -1000, 7, 8), 24),
      (Array(-2, 4, -5, 4, -5, 9, 4), 15),
      (Array(-5, -7, -1, -2, -4), -1)
    )

    val source = scala.io.Source.fromResource("maximumSubCircularSubarray.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()

    for (cond <- conditions) {
      assert(maxSubarraySumCircularBruteForce(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(maxSubarraySumCircularBruteForce(testcase) == 2126311, f"wrong for array from maximumSubCircularSubarray.txt")
      assert(maxSubarraySumCircular(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(maxSubarraySumCircular(testcase) == 2126311, f"wrong for array from maximumSubCircularSubarray.txt")
    }
  }

  test("Maximum Product Subarray") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(2, 3, -2, 4), 6),
      (Array(2, 3, -2, 4, -3), 144),
      (Array(2, 3, -2, 4, -3, 1, 2, -6, 10), 1440),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, -6, 10), 10),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, -6, 0, -6, 10), 10),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, -6, -6, -6, 0, -6, 10), 36),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, 0, -6, 10), 10),
      (Array(-10), -10),
      (Array(-2, 0, -1), 0)
    )

    for (cond <- conditions) {
      assert(maxProductNaive(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(maxProduct(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Maximum Length of Subarray With Positive Product") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(1, -2, -3, 4), 4),
      (Array(0, 1, -2, -3, -4), 3),
      (Array(-1, -2, -3, 0, 1), 2),
      (Array(2), 1),
      (Array(0), 0),
      (Array(-1), 0),
      (Array(2, 3, -2, 4), 2),
      (Array(2, 3, -2, 4, -3), 5),
      (Array(2, 3, -2, 4, -3, 1, 2, -6, 10), 7),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, -6, 10), 2),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, -6, 0, -6, 10), 2),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, -6, -6, -6, 0, -6, 10), 2),
      (Array(2, 3, -2, 4, 0, -3, 1, 2, 0, 0, -6, 10), 2),
      (Array(-10), 0),
      (Array(-2, 0, -1), 0),
      (Array(-1, 2), 1),
      (Array(-1, 2, 2, 2, 2), 4),
      (Array(2, 2, -1, 2, 2, 3), 3),
      (Array(-2, 2, -1, 2, 2, 3), 6),
      (Array(-1, 2, -1, 4), 4)
    )

    for (cond <- conditions) {
      assert(getMaxLen(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(getMaxLenKadane(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Group Anagrams") {
    val conditions: Array[(Array[String], List[List[String]])] = Array(
      (Array("eat", "tea", "tan", "ate", "nat", "bat"), List(List("bat"), List("tan", "nat"), List("eat", "tea", "ate"))),
      (Array("ab", "ba"), List(List("ab", "ba"))),
      (Array("a", "abc"), List(List("a"), List("abc")))
    )

    for (cond <- conditions) {
      assert(groupAnagrams(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Minimum Difficulty of a Job Schedule") {
    val conditions: Array[(Array[Int], Int, Int)] = Array(
      (Array(6, 5, 4, 3, 2, 1), 2, 7),
      (Array(6, 5, 10, 3, 2, 1), 2, 11),
      (Array(6, 5, 10, 3, 2, 1), 3, 13),
      (Array(6, 5, 10, 7, 2, 4, 2, 3, 2, 1), 4, 16),
      (Array(9, 9, 9), 4, -1),
      (Array(1, 1, 1), 3, 3)
    )

    for (cond <- conditions) {
      assert(minDifficultyRecursive(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._1.mkString(",")}")
      assert(minDifficulty(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Best Sightseeing Pair") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(8, 1, 5, 2, 6), 11),
      (Array(1, 2), 2),
      (Array(8, 1, 5, 2, 6, -2, 4, 2, 14), 16),
      (Array(3, 7, 2, 3), 9)
    )

    for (cond <- conditions) {
      assert(maxScoreSightseeingPair(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Best Time to Buy and Sell Stock") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(7, 1, 5, 3, 6, 4), 5),
      (Array(7, 1, 5, 3, 6, 0), 5),
      (Array(7, 6, 4, 3, 1), 0),
      (Array(7, 6, -2, 4, 1, 8, 3, 6, 4), 10),
      (Array(7), 0),
      (Array(0), 0),
    )

    for (cond <- conditions) {
      assert(maxProfitKadane(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Best Time to Buy and Sell Stock II") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(7, 1, 5, 3, 6, 4), 7),
      (Array(7, 6, 4, 3, 1), 0),
      (Array(1, 2, 3, 4, 5), 4),
      (Array(7, 6, -2, 4, 1, 8, 3, 6, 4), 16),
      (Array(7), 0),
      (Array(0), 0),
    )

    for (cond <- conditions) {
      assert(maxProfitII(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Best Time to Buy and Sell Stock with Cooldown") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(1, 2, 3, 0, 2), 3),
      (Array(3, 1, 2, 3, 0, 2), 3),
      (Array(3, 1, 2, 6, 7, 8, 1, 4, 7), 12),
      (Array(1), 0),
      (Array(1, 2), 1),
      (Array(1, 2, 3), 2),
      (Array(1, 4, 2), 3)
    )

    for (cond <- conditions) {
      assert(maxProfitCooldown(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Best Time to Buy and Sell Stock IV") {
    val conditions: Array[(Int, Array[Int], Int)] = Array(
      (2, Array(2, 4, 1), 2),
      (2, Array(3, 2, 6, 5, 0, 3), 7),
      (100, Array(70, 4, 83, 56, 94, 72, 78, 43, 2, 86, 65, 100, 94, 56, 41, 66, 3, 33, 10, 3, 45, 94, 15, 12, 78, 60, 58, 0, 58, 15, 21, 7, 11, 41, 12, 96, 83, 77, 47, 62, 27, 19, 40, 63, 30, 4, 77, 52, 17, 57, 21, 66, 63, 29, 51, 40, 37, 6, 44, 42, 92, 16, 64, 33, 31, 51, 36, 0, 29, 95, 92, 35, 66, 91, 19, 21, 100, 95, 40, 61, 15, 83, 31, 55, 59, 84, 21, 99, 45, 64, 90, 25, 40, 6, 41, 5, 25, 52, 59, 61, 51, 37, 92, 90, 20, 20, 96, 66, 79, 28, 83, 60, 91, 30, 52, 55, 1, 99, 8, 68, 14, 84, 59, 5, 34, 93, 25, 10, 93, 21, 35, 66, 88, 20, 97, 25, 63, 80, 20, 86, 33, 53, 43, 86, 53, 55, 61, 77, 9, 2, 56, 78, 43, 19, 68, 69, 49, 1, 6, 5, 82, 46, 24, 33, 85, 24, 56, 51, 45, 100, 94, 26, 15, 33, 35, 59, 25, 65, 32, 26, 93, 73, 0, 40, 92, 56, 76, 18, 2, 45, 64, 66, 64, 39, 77, 1, 55, 90, 10, 27, 85, 40, 95, 78, 39, 40, 62, 30, 12, 57, 84, 95, 86, 57, 41, 52, 77, 17, 9, 15, 33, 17, 68, 63, 59, 40, 5, 63, 30, 86, 57, 5, 55, 47, 0, 92, 95, 100, 25, 79, 84, 93, 83, 93, 18, 20, 32, 63, 65, 56, 68, 7, 31, 100, 88, 93, 11, 43, 20, 13, 54, 34, 29, 90, 50, 24, 13, 44, 89, 57, 65, 95, 58, 32, 67, 38, 2, 41, 4, 63, 56, 88, 39, 57, 10, 1, 97, 98, 25, 45, 96, 35, 22, 0, 37, 74, 98, 14, 37, 77, 54, 40, 17, 9, 28, 83, 13, 92, 3, 8, 60, 52, 64, 8, 87, 77, 96, 70, 61, 3, 96, 83, 56, 5, 99, 81, 94, 3, 38, 91, 55, 83, 15, 30, 39, 54, 79, 55, 86, 85, 32, 27, 20, 74, 91, 99, 100, 46, 69, 77, 34, 97, 0, 50, 51, 21, 12, 3, 84, 84, 48, 69, 94, 28, 64, 36, 70, 34, 70, 11, 89, 58, 6, 90, 86, 4, 97, 63, 10, 37, 48, 68, 30, 29, 53, 4, 91, 7, 56, 63, 22, 93, 69, 93, 1, 85, 11, 20, 41, 36, 66, 67, 57, 76, 85, 37, 80, 99, 63, 23, 71, 11, 73, 41, 48, 54, 61, 49, 91, 97, 60, 38, 99, 8, 17, 2, 5, 56, 3, 69, 90, 62, 75, 76, 55, 71, 83, 34, 2, 36, 56, 40, 15, 62, 39, 78, 7, 37, 58, 22, 64, 59, 80, 16, 2, 34, 83, 43, 40, 39, 38, 35, 89, 72, 56, 77, 78, 14, 45, 0, 57, 32, 82, 93, 96, 3, 51, 27, 36, 38, 1, 19, 66, 98, 93, 91, 18, 95, 93, 39, 12, 40, 73, 100, 17, 72, 93, 25, 35, 45, 91, 78, 13, 97, 56, 40, 69, 86, 69, 99, 4, 36, 36, 82, 35, 52, 12, 46, 74, 57, 65, 91, 51, 41, 42, 17, 78, 49, 75, 9, 23, 65, 44, 47, 93, 84, 70, 19, 22, 57, 27, 84, 57, 85, 2, 61, 17, 90, 34, 49, 74, 64, 46, 61, 0, 28, 57, 78, 75, 31, 27, 24, 10, 93, 34, 19, 75, 53, 17, 26, 2, 41, 89, 79, 37, 14, 93, 55, 74, 11, 77, 60, 61, 2, 68, 0, 15, 12, 47, 12, 48, 57, 73, 17, 18, 11, 83, 38, 5, 36, 53, 94, 40, 48, 81, 53, 32, 53, 12, 21, 90, 100, 32, 29, 94, 92, 83, 80, 36, 73, 59, 61, 43, 100, 36, 71, 89, 9, 24, 56, 7, 48, 34, 58, 0, 43, 34, 18, 1, 29, 97, 70, 92, 88, 0, 48, 51, 53, 0, 50, 21, 91, 23, 34, 49, 19, 17, 9, 23, 43, 87, 72, 39, 17, 17, 97, 14, 29, 4, 10, 84, 10, 33, 100, 86, 43, 20, 22, 58, 90, 70, 48, 23, 75, 4, 66, 97, 95, 1, 80, 24, 43, 97, 15, 38, 53, 55, 86, 63, 40, 7, 26, 60, 95, 12, 98, 15, 95, 71, 86, 46, 33, 68, 32, 86, 89, 18, 88, 97, 32, 42, 5, 57, 13, 1, 23, 34, 37, 13, 65, 13, 47, 55, 85, 37, 57, 14, 89, 94, 57, 13, 6, 98, 47, 52, 51, 19, 99, 42, 1, 19, 74, 60, 8, 48, 28, 65, 6, 12, 57, 49, 27, 95, 1, 2, 10, 25, 49, 68, 57, 32, 99, 24, 19, 25, 32, 89, 88, 73, 96, 57, 14, 65, 34, 8, 82, 9, 94, 91, 19, 53, 61, 70, 54, 4, 66, 26, 8, 63, 62, 9, 20, 42, 17, 52, 97, 51, 53, 19, 48, 76, 40, 80, 6, 1, 89, 52, 70, 38, 95, 62, 24, 88, 64, 42, 61, 6, 50, 91, 87, 69, 13, 58, 43, 98, 19, 94, 65, 56, 72, 20, 72, 92, 85, 58, 46, 67, 2, 23, 88, 58, 25, 88, 18, 92, 46, 15, 18, 37, 9, 90, 2, 38, 0, 16, 86, 44, 69, 71, 70, 30, 38, 17, 69, 69, 80, 73, 79, 56, 17, 95, 12, 37, 43, 5, 5, 6, 42, 16, 44, 22, 62, 37, 86, 8, 51, 73, 46, 44, 15, 98, 54, 22, 47, 28, 11, 75, 52, 49, 38, 84, 55, 3, 69, 100, 54, 66, 6, 23, 98, 22, 99, 21, 74, 75, 33, 67, 8, 80, 90, 23, 46, 93, 69, 85, 46, 87, 76, 93, 38, 77, 37, 72, 35, 3, 82, 11, 67, 46, 53, 29, 60, 33, 12, 62, 23, 27, 72, 35, 63, 68, 14, 35, 27, 98, 94, 65, 3, 13, 48, 83, 27, 84, 86, 49, 31, 63, 40, 12, 34, 79, 61, 47, 29, 33, 52, 100, 85, 38, 24, 1, 16, 62, 89, 36, 74, 9, 49, 62, 89), 8740)
    )

    for (cond <- conditions) {
      assert(maxProfitKTransactionsRecursive(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._2.mkString(",")}")
      assert(maxProfitKTransactionsIterative(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._2.mkString(",")}")
    }

    val source = scala.io.Source.fromResource("maximumSubCircularSubarray.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next().take(999)

    assert(maxProfitKTransactionsRecursive(2, testcase) == 119240)
    assert(maxProfitKTransactionsIterative(2, testcase) == 119240)
  }

  test("Coin Change") {
    val conditions: Array[(Array[Int], Int, Int)] = Array(
      (Array(1, 2, 5), 11, 3),
      (Array(2), 3, -1),
      (Array(1), 0, 0),
      (Array(3, 7, 405, 436), 8839, 25),
      (Array(389, 46, 222, 352, 4, 250), 5343, 16)
    )

    for (cond <- conditions) {
      assert(coinChange(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Coin Change 2") {
    val conditions: Array[(Array[Int], Int, Int)] = Array(
      (Array(1, 2, 5), 5, 4),
      (Array(3), 2, 0),
      (Array(10), 10, 1),
      (Array(1, 2, 5, 10, 15, 25), 100, 7572),
      (Array(10, 5), 20, 3),
    )

    for (cond <- conditions) {
      assert(change(cond._2, cond._1) == cond._3, f"wrong for array: ${cond._1.mkString(",")}")
      assert(changeFP(cond._2, cond._1) == cond._3, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Word Break") {
    val conditions: Array[(String, List[String], Boolean)] = Array(
      ("leetcode", List("leet", "code"), true),
      ("applepenapple", List("apple", "pen"), true),
      ("catsandog", List("cats", "dog", "sand", "and", "cat"), false),
      ("catsanddog", List("cats", "dog", "sand", "and", "cat"), true)
    )

    for (cond <- conditions) {
      assert(wordBreakRecursive(cond._1, cond._2) == cond._3, f"wrong for array: ${cond._1.mkString("")}")
    }
  }

  test("Longest Increasing Subsequence") {
    val conditions: Array[(Array[Int], Int)] = Array(
      (Array(10, 9, 2, 5, 3, 7, 101, 18), 4),
      (Array(0, 1, 0, 3, 2, 3), 4),
      (Array(7, 7, 7, 7, 7, 7, 7), 1),
      (Array(0, 1, 0, 3, 2, 3, 4), 5),
    )

    for (cond <- conditions) {
      assert(lengthOfLISBruteForce(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
    }
  }

  test("Paint Fence") {
    val conditions: Array[(Int, Int, Int)] = Array(
      (3, 2, 6),
      (7, 2, 42),
      (4, 3, 66),
      (43, 2, 1402817466)
    )

    for (cond <- conditions) {
      assert(numWaysRecursive(cond._1, cond._2) == cond._3, f"wrong for pair: ${cond._1}, ${cond._2}")
    }
  }

  test("Count Unique Characters of All Substrings of a Given String") {
    val conditions: Array[(String, Int)] = Array(
      ("ABC", 10),
      ("ABA", 8),
      ("LEE", 6),
      ("LEETCODE", 92),
      ("DELQGVWNZKIJJPSXOVWWIZUXCEGWSQLESNSRBMKZARFPAXSVWQEZDENDAHNNIBHGHTFDLPGDLFXMIYRFNLMXHNPIFUAXINXPXLCTTJNLGGMKJIOEWBECNOFQPVCIKIAZMNGHEHFMCPWSMJTMGVSXTOGCGUYKFMNCGLCBRAFJLJVPIVDOLJBURULPGXBVDCEWXXXLTRMSHPKSPFDGNVOCZWDXJUWVNAREDOKTZMIUDKDQWWWSAEUUDBHMWZELOSBIHMAYJEMGZPMDOOGSCKLVHTGMETHUISCLJKDOQEWGVBULEMUXGTRKGXYFDIZTZWMLOFTCANBGUARNWQEQWGMIKMORVQUZANJNRNPMJWYLVHWKDFLDDBBMILAKGFROEQAMEVONUVHOHGPKLBPNYZFPLXNBCIFENCGIMIDCXIIQJWPVVCOCJTSKSHVMQJNLHSQTEZQTTMOXUSKBMUJEJDBJQNXECJGSZUDENJCPTTSREKHPRIISXMWBUGMTOVOTRKQCFSDOTEFPSVQINYLHXYVZTVAMWGPNKIDLOPGAMWSKDXEPLPPTKUHEKBQAWEBMORRZHBLOGIYLTPMUVBPGOOOIEBJEGTKQKOUURHSEJCMWMGHXYIAOGKJXFAMRLGTPNSLERNOHSDFSSFASUJTFHBDMGBQOKZRBRAZEQQVWFRNUNHBGKRFNBETEDJIWCTUBJDPFRRVNZENGRANELPHSDJLKVHWXAXUTMPWHUQPLTLYQAATEFXHZARFAUDLIUDEHEGGNIYICVARQNRJJKQSLXKZZTFPVJMOXADCIGKUXCVMLPFJGVXMMBEKQXFNXNUWOHCSZSEZWZHDCXPGLROYPMUOBDFLQMTTERGSSGVGOURDWDSEXONCKWHDUOVDHDESNINELLCTURJHGCJWVIPNSISHRWTFSFNRAHJAJNNXKKEMESDWGIYIQQRLUUADAXOUEYURQRVZBCSHXXFLYWFHDZKPHAGYOCTYGZNPALAUZSTOU", 629134)
    )

    for (cond <- conditions) {
      assert(uniqueLetterStringFP(cond._1) == cond._2, f"wrong for pair: ${cond._1}, ${cond._2}")
    }
  }

  test("Decode Ways") {
    val conditions: Array[(String, Int)] = Array(
      ("11106", 2),
      ("13106", 2),
      ("111", 3),
      ("131", 2),
      ("12", 2),
      ("226", 3),
      ("2026", 2),
      ("1103", 1),
      ("20026", 0),
      ("06", 0),
      ("001", 0),
      ("1123", 5),
      ("11232", 5),
      ("11222", 8),
      ("109106", 1),
      ("1092106", 1),
      ("1026106", 2),
      ("111111111111111111111111111111111111111111111", 1836311903),
      ("301", 0),
      ("27", 1)
    )

    for (cond <- conditions) {
      assert(numDecodings(cond._1) == cond._2, f"wrong for pair: ${cond._1}, ${cond._2}")
    }
  }

  test("Count Number of Texts") {
    val conditions: Array[(String, Int)] = Array(
      ("22233", 8),
      ("22", 2),
      ("222", 4),
      ("2222", 7),
      ("7777", 8),
      ("22222", 13),
      ("222222", 24),
      ("2222222", 44),
      ("222223", 13),
      ("2222233", 26),
      ("22222233", 48),
      ("22222222222222222", 19513),
      ("22222222222222222222222222222", 29249425),
      ("222222222222222222222222222222222", 334745777),
      ("2222222222222222222222222222222222", 615693474),
      ("22222222222222222222222222222222222", 132436845),
      ("222222222222222222222222222222222222", 82876089),
      ("222772227772222277222299992222222222", 51064832),
      ("444444444444444444444444444444448888888888888888999999999999333333333333333366666666666666662222222222222222666666666666666633333333333333338888888888888888222222222222222244444444444444448888888888888222222222222222288888888888889999999999999999333333333444444664", 537551452)
    )

    for (cond <- conditions) {
      assert(countTexts(cond._1) == cond._2, f"wrong for pair: ${cond._1}, ${cond._2}")
    }

    val source = scala.io.Source.fromResource("countNumberOfTexts.txt")
    val testcase: String = source.getLines().map(_.split(",")).next().head
    assert(countTexts(testcase) == 818804400)

  }

  test("Cheapest Flights Within K Stops") {
    val conditions: Array[(Int, Array[Array[Int]], Int, Int, Int, Int)] = Array(
      (4, Array(Array(0, 1, 100), Array(1, 2, 100), Array(2, 0, 100), Array(1, 3, 600), Array(2, 3, 200)), 0, 3, 1, 700),
      (3, Array(Array(0, 1, 100), Array(1, 2, 100), Array(0, 2, 500)), 0, 2, 1, 200),
      (3, Array(Array(0, 1, 100), Array(1, 2, 100), Array(0, 2, 500)), 0, 2, 0, 500),
      (5, Array(Array(4, 1, 1), Array(1, 2, 3), Array(0, 3, 2), Array(0, 4, 10), Array(3, 1, 1), Array(1, 4, 3)), 2, 1, 1, -1)
    )

    for (cond <- conditions) {
      assert(findCheapestPrice(cond._1, cond._2, cond._3, cond._4, cond._5) == cond._6)
    }
  }

  test("Palindromic Substrings") {
    val conditions: Array[(String, Int)] = Array(
      ("abc", 3),
      ("aaa", 6),
      ("xabax", 7),
      ("axbobax", 8)
    )

    for (cond <- conditions) {
      assert(countSubstrings(cond._1) == cond._2)
    }
  }

  test("Ones and Zeroes") {
    val conditions: Array[(Array[String], Int, Int, Int)] = Array(
      (Array("10", "0001", "111001", "1", "0"), 5, 3, 4),
      (Array("10", "0", "1"), 1, 1, 2),
      (Array("10", "0001", "111001", "1", "0", "010001", "0", "11", "0001"), 12, 5, 6),
      (Array("0", "11", "1000", "01", "0", "101", "1", "1", "1", "0", "0", "0", "0", "1", "0", "0110101", "0", "11", "01", "00", "01111", "0011", "1", "1000", "0", "11101", "1", "0", "10", "0111"), 9, 80, 17),
      (Array("0", "11", "1000", "01", "0", "101", "1", "1", "1", "0", "0", "0", "0", "1", "0", "0110101", "0", "11", "01", "00", "01111"), 9, 80, 15),
    )

    for (cond <- conditions) {
      assert(findMaxForm(cond._1, cond._2, cond._3) == cond._4)
    }
  }

  test("Remove Palindromic Subsequences") {
    val conditions: Array[(String, Int)] = Array(
      ("ababa", 1),
      ("ab", 2),
      ("abb", 2),
      ("baabb", 2),
      ("abbbabb", 2),
      ("abbbabbbababab", 2)
    )

    for (cond <- conditions) {
      assert(removePalindromeSub(cond._1) == cond._2)
    }
  }

  test("Minimum Cost For Tickets") {
    val conditions: Array[(Array[Int], Array[Int], Int)] = Array(
      (Array(1, 4, 6, 7, 8, 20), Array(2, 7, 15), 11),
      (Array(1, 4, 6, 7, 8, 20), Array(7, 2, 15), 6),
      (Array(1, 5, 8, 9, 10, 12, 13, 16, 17, 18, 19, 20, 23, 24, 29), Array(3, 12, 54), 39),
      (Array(1, 5, 8, 9, 10, 12, 13, 16, 17, 18, 19, 20), Array(3, 12, 54), 30)
    )

    for (cond <- conditions) {
      assert(mincostTickets(cond._1, cond._2) == cond._3)
    }
  }


}