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

}
