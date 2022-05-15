import org.scalatest.{FunSuite, Matchers}
import Graph._
import datastructure.graph.Node

class GraphTest extends FunSuite with Matchers {
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

  test("All Paths From Source to Target") {
    val conditions: Array[(Array[Array[Int]], List[List[Int]])] =
      Array(
        (Array(Array(1, 2), Array(3), Array(3), Array()), List(List(0, 1, 3), List(0, 2, 3))),
        (Array(Array(4, 3, 1), Array(3, 2, 4), Array(3), Array(4), Array()), List(List(0, 4), List(0, 3, 4), List(0, 1, 3, 4), List(0, 1, 2, 3, 4), List(0, 1, 4)))
      )

    for (cond <- conditions) {
      allPathsSourceTarget(cond._1) should contain theSameElementsAs cond._2
    }
  }

  test("Clone Graph") {
    val node1 = new Node(1)
    val node2 = new Node(2)
    val node3 = new Node(3)
    val node4 = new Node(4)

    node1.neighbors = List(node2, node4)
    node2.neighbors = List(node1, node3)
    node3.neighbors = List(node2, node4)
    node4.neighbors = List(node1, node3)

    val newNode1 = cloneGraph(node1)
    assert(newNode1.neighbors.length == 2)
    newNode1.neighbors.map(_.value) should contain theSameElementsAs List(2, 4)
    assert(newNode1.neighbors.head.neighbors.length == 2)

  }

  test("Reconstruct Itinerary") {
    val conditions: Array[(List[List[String]], List[String])] =
      Array(
        (List(List("MUC", "LHR"), List("JFK", "MUC"), List("SFO", "SJC"), List("LHR", "SFO")), List("JFK", "MUC", "LHR", "SFO", "SJC")),
        (List(List("JFK", "SFO"), List("JFK", "ATL"), List("SFO", "ATL"), List("ATL", "JFK"), List("ATL", "SFO")), List("JFK", "ATL", "JFK", "SFO", "ATL", "SFO")),
        (List(List("JFK", "KUL"), List("JFK", "NRT"), List("NRT", "JFK")), List("JFK", "NRT", "JFK", "KUL")),
        (List(List("AXA", "EZE"), List("EZE", "AUA"), List("ADL", "JFK"), List("ADL", "TIA"), List("AUA", "AXA"), List("EZE", "TIA"), List("EZE", "TIA"), List("AXA", "EZE"), List("EZE", "ADL"), List("ANU", "EZE"), List("TIA", "EZE"), List("JFK", "ADL"), List("AUA", "JFK"), List("JFK", "EZE"), List("EZE", "ANU"), List("ADL", "AUA"), List("ANU", "AXA"), List("AXA", "ADL"), List("AUA", "JFK"), List("EZE", "ADL"), List("ANU", "TIA"), List("AUA", "JFK"), List("TIA", "JFK"), List("EZE", "AUA"), List("AXA", "EZE"), List("AUA", "ANU"), List("ADL", "AXA"), List("EZE", "ADL"), List("AUA", "ANU"), List("AXA", "EZE"), List("TIA", "AUA"), List("AXA", "EZE"), List("AUA", "SYD"), List("ADL", "JFK"), List("EZE", "AUA"), List("ADL", "ANU"), List("AUA", "TIA"), List("ADL", "EZE"), List("TIA", "JFK"), List("AXA", "ANU"), List("JFK", "AXA"), List("JFK", "ADL"), List("ADL", "EZE"), List("AXA", "TIA"), List("JFK", "AUA"), List("ADL", "EZE"), List("JFK", "ADL"), List("ADL", "AXA"), List("TIA", "AUA"), List("AXA", "JFK"), List("ADL", "AUA"), List("TIA", "JFK"), List("JFK", "ADL"), List("JFK", "ADL"), List("ANU", "AXA"), List("TIA", "AXA"), List("EZE", "JFK"), List("EZE", "AXA"), List("ADL", "TIA"), List("JFK", "AUA"), List("TIA", "EZE"), List("EZE", "ADL"), List("JFK", "ANU"), List("TIA", "AUA"), List("EZE", "ADL"), List("ADL", "JFK"), List("ANU", "AXA"), List("AUA", "AXA"), List("ANU", "EZE"), List("ADL", "AXA"), List("ANU", "AXA"), List("TIA", "ADL"), List("JFK", "ADL"), List("JFK", "TIA"), List("AUA", "ADL"), List("AUA", "TIA"), List("TIA", "JFK"), List("EZE", "JFK"), List("AUA", "ADL"), List("ADL", "AUA"), List("EZE", "ANU"), List("ADL", "ANU"), List("AUA", "AXA"), List("AXA", "TIA"), List("AXA", "TIA"), List("ADL", "AXA"), List("EZE", "AXA"), List("AXA", "JFK"), List("JFK", "AUA"), List("ANU", "ADL"), List("AXA", "TIA"), List("ANU", "AUA"), List("JFK", "EZE"), List("AXA", "ADL"), List("TIA", "EZE"), List("JFK", "AXA"), List("AXA", "ADL"), List("EZE", "AUA"), List("AXA", "ANU"), List("ADL", "EZE"), List("AUA", "EZE")), List("JFK", "ADL", "ANU", "ADL", "ANU", "AUA", "ADL", "AUA", "ADL", "AUA", "ANU", "AXA", "ADL", "AUA", "ANU", "AXA", "ADL", "AXA", "ADL", "AXA", "ANU", "AXA", "ANU", "AXA", "EZE", "ADL", "AXA", "EZE", "ADL", "AXA", "EZE", "ADL", "EZE", "ADL", "EZE", "ADL", "EZE", "ANU", "EZE", "ANU", "EZE", "AUA", "AXA", "EZE", "AUA", "AXA", "EZE", "AUA", "AXA", "JFK", "ADL", "EZE", "AUA", "EZE", "AXA", "JFK", "ADL", "JFK", "ADL", "JFK", "ADL", "JFK", "ADL", "TIA", "ADL", "TIA", "AUA", "JFK", "ANU", "TIA", "AUA", "JFK", "AUA", "JFK", "AUA", "TIA", "AUA", "TIA", "AXA", "TIA", "EZE", "AXA", "TIA", "EZE", "JFK", "AXA", "TIA", "EZE", "JFK", "AXA", "TIA", "JFK", "EZE", "TIA", "JFK", "EZE", "TIA", "JFK", "TIA", "JFK", "AUA", "SYD"))
      )

    for (cond <- conditions) {
      assert(findItinerary(cond._1) == cond._2)
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
