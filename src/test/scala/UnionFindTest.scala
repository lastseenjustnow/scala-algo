import org.scalatest.FunSuite
import DisjointSet._
import datastructure.graph.QuickUnion


class UnionFindTest extends FunSuite {
  test("Test Union Find data structure") {
    val uf = new QuickUnion(10)
    uf.union(1, 2)
    uf.union(2, 5)
    uf.union(5, 6)
    uf.union(6, 7)
    uf.union(3, 8)
    uf.union(8, 9)
    assert(uf.connected(1, 5))
    assert(uf.connected(5, 7))
    assert(!uf.connected(4, 9))
    uf.union(9, 4)
    assert(uf.connected(4, 9))
  }

  test("Number of Provinces") {
    val conditions: Array[(Array[Array[Int]], Int)] =
      Array(
        (Array(Array(1, 1, 0), Array(1, 1, 0), Array(0, 0, 1)), 2),
        (Array(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1)), 3),
        (Array(Array(1, 1, 1), Array(1, 1, 1), Array(1, 1, 1)), 1),
      )

    for (cond <- conditions) {
      assert(findCircleNum(cond._1) == cond._2)
    }
  }

  test("Graph Valid Tree") {
    val conditions: Array[(Array[Array[Int]], Int, Boolean)] =
      Array(
        (Array(Array(0, 1), Array(0, 2), Array(0, 3), Array(1, 4)), 5, true),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(1, 3), Array(1, 4)), 5, false),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(1, 3), Array(1, 4), Array(6, 7)), 7, false),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(0, 4), Array(0, 5), Array(1, 6), Array(6, 7)), 8, true),
        (Array(Array(0, 1), Array(2, 3), Array(1, 2)), 4, true)
      )

    for (cond <- conditions) {
      assert(validTree(cond._2, cond._1) == cond._3)
    }
  }

  test("Number of Connected Components in an Undirected Graph") {
    val conditions: Array[(Array[Array[Int]], Int, Int)] =
      Array(
        (Array(Array(0, 1), Array(1, 2), Array(3, 4)), 5, 2),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(3, 4)), 5, 1)
      )

    for (cond <- conditions) {
      assert(countComponents(cond._2, cond._1) == cond._3)
    }
  }

  test("The Earliest Moment When Everyone Become Friends") {
    val conditions: Array[(Array[Array[Int]], Int, Int)] =
      Array(
        (Array(Array(20190101, 0, 1), Array(20190104, 3, 4), Array(20190107, 2, 3), Array(20190211, 1, 5), Array(20190224, 2, 4), Array(20190301, 0, 3), Array(20190312, 1, 2), Array(20190322, 4, 5)), 6, 20190301),
        (Array(Array(0, 2, 0), Array(1, 0, 1), Array(3, 0, 3), Array(4, 1, 2), Array(7, 3, 1)), 4, 3),
        (Array(Array(9, 3, 0), Array(0, 2, 1), Array(8, 0, 1), Array(1, 3, 2), Array(2, 2, 0), Array(3, 3, 1)), 4, 2)
      )

    for (cond <- conditions) {
      assert(earliestAcq(cond._1, cond._2) == cond._3)
    }
  }

  test("Smallest String With Swaps") {
    val conditions: Array[(String, List[List[Int]], String)] =
      Array(
        ("dcab", List(List(0, 3), List(1, 2)), "bacd"),
        ("dcab", List(List(0, 3), List(1, 2), List(0, 2)), "abcd"),
        ("cba", List(List(0, 1), List(1, 2)), "abc")
      )

    for (cond <- conditions) {
      assert(smallestStringWithSwaps(cond._1, cond._2) == cond._3)
    }
  }

  test("Evaluate Division") {
    val conditions: Array[(List[List[String]], Array[Double], List[List[String]], Array[Double])] =
      Array(
        (List(List("a", "b"), List("b", "c")), Array(2.0, 3.0), List(List("a", "c"), List("b", "a"), List("a", "e"), List("a", "a"), List("x", "x")), Array(6.00000, 0.50000, -1.00000, 1.00000, -1.00000)),
        (List(List("a", "b"), List("b", "c"), List("bc", "cd")), Array(1.5, 2.5, 5.0), List(List("a", "c"), List("c", "b"), List("bc", "cd"), List("cd", "bc")), Array(3.75000, 0.40000, 5.00000, 0.20000)),
        (List(List("a", "b")), Array(0.5), List(List("a", "b"), List("b", "a"), List("a", "c"), List("x", "y")), Array(0.5, 2.00000, -1.00000, -1.00000)),
        (List(List("x1", "x2"), List("x2", "x3"), List("x1", "x4"), List("x2", "x5")), Array(3.0, 0.5, 3.4, 5.6), List(List("x2", "x4"), List("x1", "x5"), List("x1", "x3"), List("x5", "x5"), List("x5", "x1"), List("x3", "x4"), List("x4", "x3"), List("x6", "x6"), List("x0", "x0")), Array(1.1333333333333333, 16.8, 1.5, 1.0, 0.05952380952380953, 2.2666666666666666, 0.44117647058823534, -1.0, -1.0)),
        (List(List("a", "e"), List("b", "e")), Array(4.0, 3.0), List(List("a", "b"), List("e", "e"), List("x", "x")), Array(1.3333333333333333, 1.00000, -1.00000)),
        (List(List("a", "b"), List("e", "f"), List("b", "e")), Array(3.4, 1.4, 2.3), List(List("b", "a"), List("a", "f"), List("f", "f"), List("e", "e"), List("c", "c"), List("a", "c"), List("f", "e")), Array(0.29411764705882354, 10.947999999999999, 1.0, 1.0, -1.0, -1.0, 0.7142857142857143)),
        (List(List("a", "b"), List("c", "d")), Array(1.0, 1.0), List(List("a", "c"), List("b", "d"), List("b", "a"), List("d", "c")), Array(-1.00000, -1.00000, 1.00000, 1.00000)),
        (List(List("a", "b"), List("c", "b"), List("d", "b"), List("w", "x"), List("y", "x"), List("z", "x"), List("w", "d")), Array(2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), List(List("a", "c"), List("b", "c"), List("a", "e"), List("a", "a"), List("x", "x"), List("a", "z")), Array(0.6666666666666666, 0.3333333333333333, -1.00000, 1.00000, 1.00000, 0.04464285714285714))
      )

    for (cond <- conditions) {
      assert(calcEquation(cond._1, cond._2, cond._3).toList == cond._4.toList)
    }
  }

  test("Optimize Water Distribution in a Village") {
    val conditions: Array[(Int, Array[Int], Array[Array[Int]], Int)] =
      Array(
        (3, Array(1, 2, 2), Array(Array(1, 2, 1), Array(2, 3, 1)), 3),
        (2, Array(1, 1), Array(Array(1, 2, 1), Array(1, 2, 2)), 2),
        (5, Array(7, 9, 14, 2, 3), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 13),
        (5, Array(7, 9, 1, 2, 3), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 12),
        (6, Array(7, 9, 1, 2, 3, 8), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 20),
        (6, Array(7, 1, 1, 2, 3, 8), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 19),
        (7, Array(7, 1, 1, 2, 3, 8, 3), Array(Array(7, 4, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 25),
        (5, Array(46012, 72474, 64965, 751, 33304), Array(Array(2, 1, 6719), Array(3, 2, 75312), Array(5, 3, 44918)), 131704),
      )

    for (cond <- conditions) {
      assert(minCostToSupplyWater(cond._1, cond._2, cond._3) == cond._4)
    }
  }

}
