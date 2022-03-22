import org.scalatest.FunSuite
import DynamicProgramming._


class DynamicProgrammingTest extends FunSuite {
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

  test("Fibonacci Number") {
    val conditions = Array((-1, 0), (0, 0), (2, 1), (3, 2), (4, 3), (20, 6765))

    for (cond <- conditions) {
      assert(fib(cond._1) == cond._2)
    }
  }

  test("N-th Tribonacci Number") {
    val conditions = Array((3, 2), (4, 4), (25, 1389537))

    for (cond <- conditions) {
      assert(tribonacci(cond._1) == cond._2)
      assert(tribonacciRecursive(cond._1) == cond._2)
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
      assert(canJump(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
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
      assert(canJump2(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
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

    val source = scala.io.Source.fromResource("testcase.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()

    for (cond <- conditions) {
      assert(maxSubarraySumCircularBruteForce(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(maxSubarraySumCircularBruteForce(testcase) == 2126311, f"wrong for array from testcase.txt")
      assert(maxSubarraySumCircular(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
      assert(maxSubarraySumCircular(testcase) == 2126311, f"wrong for array from testcase.txt")
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
      (Array(-2, 0, -1), 0)
    )

    for (cond <- conditions) {
      assert(getMaxLen(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
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
      (Array(7, 6, 4, 3, 1), 0),
      (Array(7, 6, -2, 4, 1, 8, 3, 6, 4), 10),
      (Array(7), 0),
      (Array(0), 0),
    )

    for (cond <- conditions) {
      assert(maxProfitNaive(cond._1) == cond._2, f"wrong for array: ${cond._1.mkString(",")}")
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

}