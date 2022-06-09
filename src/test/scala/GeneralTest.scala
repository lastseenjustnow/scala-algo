import org.scalatest.FunSuite
import org.scalatest._
import General._


class GeneralTest extends FunSuite with Matchers {
  test("Campus Bikes") {
    val conditions: Array[(Array[Array[Int]], Array[Array[Int]], Array[Int])] =
      Array(
        (Array(
          Array(0, 0),
          Array(2, 1)),

          Array(
            Array(1, 2),
            Array(3, 3)),

          Array(1, 0)),

        (Array(
          Array(0, 0),
          Array(1, 1),
          Array(2, 0)),

          Array(
            Array(1, 0),
            Array(2, 2),
            Array(2, 1)),

          Array(0, 2, 1))
      )


    for (cond <- conditions) {
      assignBikes(cond._1, cond._2) should equal(cond._3)
    }
  }

  test("Divide Array Into Equal Pairs") {
    val conditions: Array[(Array[Int], Boolean)] =
      Array(
        (Array(3, 2, 3, 2, 2, 2), true),
        (Array(1, 2, 3, 4), false),
        (Array(1), false),
        (Array(0, 1, 0, 1), true)
      )


    for (cond <- conditions) {
      assert(divideArray(cond._1) == cond._2)
    }
  }

  test("Maximize Number of Subsequences in a String") {
    val conditions: Array[(String, String, Long)] =
      Array(
        ("abdcdbc", "ac", 4),
        ("abdcdbc", "ae", 1),
        ("ababb", "ab", 8),
        ("aabbb", "ab", 9),
        ("aabb", "ab", 6),
        ("bbaa", "ab", 2),
        ("abab", "aa", 3),
        ("a", "ab", 1),
        ("a", "cb", 0),
        ("zzzz", "zz", 10),
        ("vnedkpkkyxelxqptfwuzcjhqmwagvrglkeivowvbjdoyydnjrqrqejoyptzoklaxcjxbrrfmpdxckfjzahparhpanwqfjrpbslsyiwbldnpjqishlsuagevjmiyktgofvnyncizswldwnngnkifmaxbmospdeslxirofgqouaapfgltgqxdhurxljcepdpndqqgfwkfiqrwuwxfamciyweehktaegynfumwnhrgrhcluenpnoieqdivznrjljcotysnlylyswvdlkgsvrotavnkifwmnvgagjykxgwaimavqsxuitknmbxppgzfwtjdvegapcplreokicxcsbdrsyfpustpxxssnouifkypwqrywprjlyddrggkcglbgcrbihgpxxosmejchmzkydhquevpschkpyulqxgduqkqgwnsowxrmgqbmltrltzqmmpjilpfxocflpkwithsjlljxdygfvstvwqsyxlkknmgpppupgjvfgmxnwmvrfuwcrsadomyddazlonjyjdeswwznkaeaasyvurpgyvjsiltiykwquesfjmuswjlrphsdthmuqkrhynmqnfqdlwnwesdmiiqvcpingbcgcsvqmsmskesrajqwmgtdoktreqssutpudfykriqhblntfabspbeddpdkownehqszbmddizdgtqmobirwbopmoqzwydnpqnvkwadajbecmajilzkfwjnpfyamudpppuxhlcngkign", "rr", 496)
      )

    for (cond <- conditions) {
      assert(maximumSubsequenceCount(cond._1, cond._2) == cond._3)
    }

    val source = scala.io.Source.fromResource("maximizeNumberOfSubsequencesInAString.txt")
    val testcase: String = source.getLines().map(_.split(",")).next().head

    assert(maximumSubsequenceCount(testcase, "zz") == 5000050000L)
  }

  test("Minimum Operations to Halve Array Sum") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(5, 19, 8, 1), 3),
        (Array(3, 8, 20), 3),
        (Array(32, 98, 23, 14, 67, 40, 26, 9, 96, 96, 91, 76, 4, 40, 42, 2, 31, 13, 16, 37, 62, 2, 27, 25, 100, 94, 14, 3, 48, 56, 64, 59, 33, 10, 74, 47, 73, 72, 89, 69, 15, 79, 22, 18, 53, 62, 20, 9, 76, 64), 36),
        (Array(6, 58, 10, 84, 35, 8, 22, 64, 1, 78, 86, 71, 77), 9),
        (Array(1), 1),
        (Array(1, 2), 2)
      )

    for (cond <- conditions) {
      assert(halveArray(cond._1) == cond._2)
    }

    val source = scala.io.Source.fromResource("minimumOperationsToHalveArraySum.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()

    assert(halveArray(testcase) == 10020)
  }

  test("Two Sum II - Input Array Is Sorted") {
    val conditions: Array[(Array[Int], Int, Array[Int])] =
      Array(
        (Array(2, 7, 11, 15), 9, Array(1, 2)),
        (Array(2, 7, 11, 15), 18, Array(2, 3)),
        (Array(2, 3, 4), 6, Array(1, 3)),
        (Array(-1, 0), -1, Array(1, 2)),
        (Array(1, 2, 3, 4, 4, 9, 56, 90), 8, Array(4, 5))
      )

    for (cond <- conditions) {
      assert(twoSumSortedRecursive(cond._1, cond._2) sameElements cond._3, f"wrong for array: ${cond._1.mkString("")}")
      assert(twoSumSortedIterative(cond._1, cond._2) sameElements cond._3, f"wrong for array: ${cond._1.mkString("")}")
    }
  }

  test("Three Sum") {
    val conditions: Array[(Array[Int], List[List[Int]])] =
      Array(
        (Array(-1, 0, 1, 2, -1, -4), List(List(-1, 0, 1), List(-1, -1, 2))),
        (Array(-1, 0, 1, 2, -1, -1, -4), List(List(-1, 0, 1), List(-1, -1, 2))),
        (Array(-1, 0, 1, 2, -1, -2, -2, -2, -1, -4), List(List(-2, 0, 2), List(-1, -1, 2), List(-1, 0, 1))),
        (Array(), List()),
        (Array(0), List()),
        (Array(0, 0, 0), List(List(0, 0, 0))),
      )

    for (cond <- conditions) {
      threeSumTwoPointers(cond._1) should contain theSameElementsAs cond._2
      threeSumHashSet(cond._1) should contain theSameElementsAs cond._2
      threeSumNoSort(cond._1) should contain theSameElementsAs cond._2
    }

    val source = scala.io.Source.fromResource("threeSum.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()

    assert(threeSumTwoPointers(testcase).length == 16258)
    assert(threeSumHashSet(testcase).length == 16258)
    assert(threeSumNoSort(testcase).length == 16258)

  }

  test("3Sum With Multiplicity") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), 8, 20),
        (Array(1, 1, 2, 2, 2, 2), 5, 12),
        (Array(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5), 9, 29),
        (Array(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5), 9, 40),
        (Array(1, 1, 2, 2, 3, 3, 3, 3, 2, 4, 4, 5, 5), 9, 52),
        (Array(28, 59, 96, 22, 40, 74, 3, 23, 1, 30, 83, 85, 3, 59, 71, 76, 99, 63, 56, 71, 38, 58, 19, 16, 47, 2, 43, 48, 58, 84, 53, 57, 38, 46, 36, 3, 84, 34, 99, 91, 21, 60, 97, 46, 36, 69, 83, 50, 42, 18, 11, 47, 86, 94, 38, 99, 95, 67, 24, 92, 22, 38, 45, 33, 2, 94, 39, 94, 80, 9, 6, 15, 56, 97, 93, 78, 27, 65, 7, 58, 20, 23, 13, 70, 73, 61, 95, 52, 2, 7, 92, 35, 38, 76, 47, 26, 66, 97, 83, 80, 100), 100, 749),
        ((for (_ <- 0 until 3000) yield 0).toArray, 0, 495500972)
      )

    for (cond <- conditions) {
      assert(threeSumMulti(cond._1, cond._2) == cond._3)
    }
  }

  test("Minimum Average Difference") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(2, 5, 3, 9, 5, 3), 3),
        (Array(0), 0),
        (Array(), 0),
        (Array(0, 1, 0, 1, 0, 1), 0),
        (Array(4, 2, 0), 2)
      )

    for (cond <- conditions) {
      assert(minimumAverageDifference(cond._1) == cond._2)
      assert(minimumAverageDifferenceFunctional(cond._1) == cond._2)
    }

    val source = scala.io.Source.fromResource("minimumAverageDifference.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()
    assert(minimumAverageDifference(testcase) == 29403)
    assert(minimumAverageDifferenceFunctional(testcase) == 29403)

  }

  test("Find the K-Beauty of a Number") {
    val conditions: Array[(Int, Int, Int)] =
      Array(
        (430043, 2, 2)
      )

    for (cond <- conditions) {
      assert(divisorSubstrings(cond._1, cond._2) == cond._3)
    }
  }

  test("Maximum White Tiles Covered by a Carpet") {
    val conditions: Array[(Array[Array[Int]], Int, Int)] =
      Array(
        (Array(Array(1, 5), Array(10, 11), Array(12, 18), Array(20, 25), Array(30, 32)), 10, 9),
        (Array(Array(1, 5), Array(10, 11), Array(13, 18), Array(20, 25), Array(30, 32)), 10, 9),
        (Array(Array(1, 1), Array(2, 2), Array(5, 5)), 2, 2),
        (Array(Array(1, 1000000000)), 1000000000, 1000000000),
        (Array(Array(3477, 3497), Array(2650, 2654), Array(2264, 2266), Array(2582, 2599), Array(2846, 2863), Array(2346, 2364), Array(3839, 3842), Array(3926, 3935), Array(2995, 3012), Array(3152, 3167), Array(4133, 4134), Array(4048, 4058), Array(3719, 3730), Array(2498, 2510), Array(2277, 2295), Array(4117, 4128)), 1638, 166),
        (Array(Array(3745, 3757), Array(3663, 3681), Array(3593, 3605), Array(3890, 3903), Array(3529, 3539), Array(3684, 3686), Array(3023, 3026), Array(2551, 2569), Array(3776, 3789), Array(3243, 3256), Array(3477, 3497), Array(2650, 2654), Array(2264, 2266), Array(2582, 2599), Array(2846, 2863), Array(2346, 2364), Array(3839, 3842), Array(3926, 3935), Array(2995, 3012), Array(3152, 3167), Array(4133, 4134), Array(4048, 4058), Array(3719, 3730), Array(2498, 2510), Array(2277, 2295), Array(4117, 4128), Array(3043, 3054), Array(3394, 3402), Array(3921, 3924), Array(3500, 3514), Array(2789, 2808), Array(3291, 3294), Array(2873, 2881), Array(2760, 2760), Array(3349, 3362), Array(2888, 2899), Array(3802, 3822), Array(3540, 3542), Array(3128, 3142), Array(2617, 2632), Array(3979, 3994), Array(2780, 2781), Array(3213, 3233), Array(3099, 3113), Array(3646, 3651), Array(3956, 3963), Array(2674, 2691), Array(3860, 3873), Array(3363, 3370), Array(2727, 2737), Array(2453, 2471), Array(4011, 4031), Array(3566, 3577), Array(2705, 2707), Array(3560, 3565), Array(3454, 3456), Array(3655, 3660), Array(4100, 4103), Array(2382, 2382), Array(4032, 4033), Array(2518, 2531), Array(2739, 2749), Array(3067, 3079), Array(4068, 4074), Array(2297, 2312), Array(2489, 2490), Array(2954, 2974), Array(2400, 2418), Array(3271, 3272), Array(3628, 3632), Array(3372, 3377), Array(2920, 2940), Array(3315, 3330), Array(3417, 3435), Array(4146, 4156), Array(2324, 2340), Array(2426, 2435), Array(2373, 2376), Array(3621, 3626), Array(2826, 2832), Array(3937, 3949), Array(3178, 3195), Array(4081, 4082), Array(4092, 4098), Array(3688, 3698)), 1638, 822)
      )

    for (cond <- conditions) {
      assert(maximumWhiteTiles(cond._1, cond._2) == cond._3)
    }
  }

  test("Check If a String Contains All Binary Codes of Size K") {
    val conditions: Array[(String, Int, Boolean)] =
      Array(
        ("00110110", 2, true),
        ("0110", 1, true),
        ("0110", 2, false),
        ("00110", 2, true),
        ("0", 10, false),
        ("0", 20, false),
        ("011000000101010100111101010101101010101010101001010101001101011110110101010101010101010100101111111010100100001010101010010010101001010000", 4, true),
        ("011000000101010100111101010101101010101010101001010101001101011110110101010101010101010100101111111010100100001010101010010010101001010000", 5, false)
      )

    for (cond <- conditions) {
      assert(hasAllCodes(cond._1, cond._2) == cond._3, f"wrong for string: ${cond._1}")
      assert(hasAllCodesSimple(cond._1, cond._2) == cond._3, f"wrong for string: ${cond._1}")
    }
  }

  test("Minimum Knight Moves") {
    val conditions: Array[(Int, Int, Int)] =
      Array(
        (1, 2, 1),
        (0, 3, 3),
        (5, 5, 4),
        (7, 7, 6),
        (15, 15, 10),
        (50, -50, 34),
        (150, -150, 100),
        (130, -86, 72)
      )

    for (cond <- conditions) {
      assert(minKnightMoves(cond._1, cond._2) == cond._3)
    }
  }

}
