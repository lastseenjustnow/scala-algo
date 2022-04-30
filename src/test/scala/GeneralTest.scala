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



}
