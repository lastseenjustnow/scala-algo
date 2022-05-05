import org.scalatest.FunSuite

import datastructure.UnionFind
import DisjointSet._


class UnionFindTest extends FunSuite {
  test("Test Union Find data structure") {
    val uf = new UnionFind(10)
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
}
