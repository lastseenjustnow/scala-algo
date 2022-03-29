import ArraysAndStrings._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ArraysAndStringsTest extends FunSuite {

  test("Two Sum") {
    val conditions: Array[(Array[Int], Int, Array[Int])] =
      Array(
        (Array(2, 7, 11, 15), 9, Array(0, 1)),
        (Array(3, 2, 4), 6, Array(1, 2)),
        (Array(3, 3), 6, Array(0, 1)),
      )

    for (cond <- conditions) {
      assert(twoSum(cond._1, cond._2) sameElements cond._3)
      assert(twoSumReturn(cond._1, cond._2) sameElements cond._3)
    }

    val testcase = (Array(2, 5, 3, 4, 6, 11), 7, Array((2, 5), (3, 4)))
    assert(twoSumBuffer(testcase._1, testcase._2) sameElements testcase._3)

  }

  test("String to Integer (atoi)") {
    val conditions: Array[(String, Int)] =
      Array(
        ("42", 42),
        ("   -42", -42),
        ("4193 with words", 4193),
        ("", 0),
        ("4193 w26", 4193),
        (".4193 w26", 0),
        (". 4193 w26", 0),
        ("words and 987", 0),
        ("-432+", -432),
        ("-91283472332", Int.MinValue),
        ("91283472332", Int.MaxValue),
        ("+-12", 0),
        ("-0032", -32),
        ("20000000000000000000", Int.MaxValue)
      )

    for (cond <- conditions) {
      assert(myAtoi(cond._1) == cond._2)
    }
  }

  test("Insert Interval") {
    val conditions: Array[(Array[Array[Int]], Array[Int], Array[Array[Int]])] =
      Array(
        (Array(Array(1, 3), Array(6, 9)), Array(2, 5), Array(Array(1, 5), Array(6, 9))),
        (Array(Array(1, 2), Array(3, 5), Array(6, 7), Array(8, 10), Array(12, 16)), Array(4, 8), Array(Array(1, 2), Array(3, 10), Array(12, 16))),
        (Array(Array(1, 3), Array(4, 6), Array(7, 9)), Array(2, 5), Array(Array(1, 6), Array(7, 9))),
        (Array(Array(1, 3), Array(5, 5), Array(4, 6), Array(7, 9)), Array(2, 5), Array(Array(1, 6), Array(7, 9))),
        (Array(), Array(2, 5), Array(Array(2, 5))),
        (Array(Array(1, 2)), Array(2, 5), Array(Array(1, 5))),
        (Array(Array(1, 2)), Array(3, 5), Array(Array(1, 2), Array(3, 5))),
        (Array(Array(1, 3), Array(6, 9)), Array(4, 5), Array(Array(1, 3), Array(4, 5), Array(6, 9))),
      )

    for (cond <- conditions) {
      insert(cond._1, cond._2) should equal(cond._3)
    }
  }


  test("K-diff Pairs in an Array") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 1, 4, 1, 5), 2, 2),
        (Array(1, 2, 3, 4, 5), 1, 4),
        (Array(1, 3, 1, 5, 4), 0, 1),
        (Array(1), 1, 0),
        (Array(1, 3, 1, 0, 5, 4, -1, 2), 2, 5),
        (Array(1, 2, 4, 4, 3, 3, 0, 9, 2, 3), 3, 2)
      )

    for (cond <- conditions) {
      findPairs(cond._1, cond._2) should equal(cond._3)
    }
  }

  test("Minimum Space Wasted From Packaging") {
    val conditions: Array[(Array[Int], Array[Array[Int]], Int)] =
      Array(
        (Array(1, 3, 4, 6, 8, 9, 10, 12), Array(Array(5, 8, 12)), 14),
        (Array(1, 3, 4, 6, 6, 8, 9, 10, 12), Array(Array(5, 8, 12)), 16),
        (Array(1, 3, 4, 6, 8, 6, 9, 8, 10, 12), Array(Array(5, 8, 13)), 19),
        (Array(1, 3, 4, 6, 8, 6, 9, 10, 12), Array(Array(5, 8, 13)), 19),
        (Array(1, 3, 4, 6, 8, 9, 10, 12), Array(Array(5, 8, 13), Array(12, 8, 8, 5), Array(5, 8, 8, 12), Array(5, 8, 12)), 14),
        (Array(2, 3, 5), Array(Array(4, 8), Array(2, 8)), 6),
        (Array(2, 3, 5), Array(Array(1, 4), Array(2, 3), Array(3, 4)), -1),
        (Array(3, 5, 8, 10, 11, 12), Array(Array(12), Array(11, 9), Array(10, 5, 14)), 9),
        (Array(1), Array(Array(12), Array(11, 9), Array(10, 5, 14)), 4),
        (Array(1), Array(Array(2), Array(3)), 1),
        (Array(7, 6, 5, 3, 4), Array(Array(2, 7), Array(6), Array(10, 5)), 10)
      )

    for (cond <- conditions) {
      minWastedSpaceBruteForce(cond._1, cond._2) should equal(cond._3)
      minWastedSpace(cond._1, cond._2) should equal(cond._3)
    }

    val testcase = ((for (i <- 1 to 100000) yield i).toArray, (for (i <- 50000 until 100000) yield Array(i, 100000)).toArray, 499949986)
    minWastedSpace(testcase._1, testcase._2) should equal(testcase._3)

  }

  test("Longest Substring Without Repeating Characters") {
    val conditions: Array[(String, Int)] =
      Array(
        ("abcabcbb", 3),
        ("bbbbb", 1),
        ("pwwkew", 3),
        ("", 0),
        ("sadfksksajkajgss", 5)
      )

    for (cond <- conditions) {
      assert(lengthOfLongestSubstring(cond._1) == cond._2)
    }
  }

  test("Container With Most Water") {
    val conditions: Array[(Array[Int], Int)] =
      Array(
        (Array(1, 8, 6, 2, 5, 4, 8, 3, 7), 49),
        (Array(1, 1), 1),
        (Array(1, 8, 6, 2, 1000, 1000, 8, 3, 7), 1000),
        (Array(1, 8, 6, 2, 1000, 4, 8, 3, 7), 49),
        (Array(1, 8, 6, 2, 1000, 2, 1000, 8, 3, 7, 15, 20, 25, 30, 40, 30), 2000),
        (Array(1, 2, 3, 4), 4),
        (Array(1, 5, 4, 1, 1, 1, 1, 1, 1, 1), 9),
        (Array(1, 5, 4, 1, 1, 1, 1, 1, 2, 1), 14),
        (Array(3, 9, 3, 4, 7, 2, 12, 6), 45),
        ((for (i <- 0 to 1000) yield i).toArray, 250000),
      )

    val source = scala.io.Source.fromResource("containerWithMostWater.txt")
    val testcase: Array[Int] = source.getLines().map(_.split(",").map(_.trim.toInt)).next()

    for (cond <- conditions) {
      assert(maxAreaBruteForce(cond._1) == cond._2)
      assert(maxArea(cond._1) == cond._2)
      assert(maxArea(testcase) == 887155335)
    }
  }

  test("Remove Element") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 2, 2, 3), 3, 2),
        (Array(0, 1, 2, 2, 3, 0, 4, 2), 2, 5),
        (Array(), 3, 0),
      )

    for (cond <- conditions) {
      assert(removeElement(cond._1, cond._2) == cond._3)
    }
  }

  test("Integer to Roman") {
    val conditions: Array[(Int, String)] =
      Array(
        (3, "III"),
        (58, "LVIII"),
        (1994, "MCMXCIV"),
        (1494, "MCDXCIV"),
        (3999, "MMMCMXCIX"),
      )

    for (cond <- conditions) {
      assert(intToRoman(cond._1) == cond._2)
    }
  }

}
