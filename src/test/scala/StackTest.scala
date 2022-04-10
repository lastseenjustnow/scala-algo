import org.scalatest.FunSuite
import Stack._

class StackTest extends FunSuite {

  test("Last Stone Weight") {
    val conditions: Array[(Array[String], Int)] =
      Array(
        (Array("5", "2", "C", "D", "+"), 30),
        (Array("5", "-2", "4", "C", "D", "9", "+", "+"), 27)
      )


    for (cond <- conditions) {
      assert(calPoints(cond._1) == cond._2)
    }
  }
}
