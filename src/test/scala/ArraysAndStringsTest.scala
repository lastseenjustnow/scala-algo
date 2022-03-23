import ArraysAndStrings._
import org.scalatest.FunSuite

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

}
