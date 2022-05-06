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

}
