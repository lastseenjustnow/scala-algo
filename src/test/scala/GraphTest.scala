import org.scalatest.FunSuite

import Graph._

class GraphTest extends FunSuite {
  test("Find if Path Exists in Graph") {
    val conditions: Array[(Int, Array[Array[Int]], Int, Int, Boolean)] =
      Array(
        (3, Array(Array(0, 1), Array(1, 2), Array(2, 0)), 0, 2, true),
        (6, Array(Array(0, 1), Array(0, 2), Array(3, 5), Array(5, 4), Array(4, 3)), 0, 5, false),
        (1, Array(), 0, 0, true),
        (10, Array(Array(0, 7), Array(0, 8), Array(6, 1), Array(2, 0), Array(0, 4), Array(5, 8), Array(4, 7), Array(1, 3), Array(3, 5), Array(6, 5)), 7, 5, true)
      )


    for (cond <- conditions) {
      assert(validPath(cond._1, cond._2, cond._3, cond._4) == cond._5)
    }
  }

  test("Network Delay Time") {
    val conditions: Array[(Array[Array[Int]], Int, Int, Int)] =
      Array(
        (Array(Array(2, 1, 1), Array(2, 3, 1), Array(3, 4, 1)), 4, 2, 2),
        (Array(Array(1, 2, 1)), 2, 1, 1),
        (Array(Array(1, 2, 1)), 2, 2, -1),
        (Array(Array(1, 2, 1), Array(2, 3, 2), Array(1, 3, 4)), 3, 1, 3)
      )

    for (cond <- conditions) {
      //assert(networkDelayTime(cond._1, cond._2, cond._3) == cond._4)
    }

    val source = scala.io.Source.fromResource("networkDelayTime.txt")
    val testcase: Array[Array[Int]] = source.getLines().map(_.split(s"],\\[")).next().map(_.split(",").map(_.toInt))
    //assert(networkDelayTime(testcase, 50, 22) == 1)

  }
}
