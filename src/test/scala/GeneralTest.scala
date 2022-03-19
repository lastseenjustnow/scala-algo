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
}
