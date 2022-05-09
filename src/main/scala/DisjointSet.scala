import datastructure.graph.{QuickFind, QuickUnion}

import scala.collection.mutable

object DisjointSet {
  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    val n = isConnected.length
    var res = n
    val uf = new QuickUnion(n)

    for (i <- 0 until n) {
      for (j <- i + 1 until n) {
        if (!uf.connected(i, j)) {
          if (isConnected(i)(j) == 1) {
            uf.union(i, j)
            res -= 1
          }
        }
      }
    }
    res
  }

  def validTree(n: Int, edges: Array[Array[Int]]): Boolean = {

    // In the valid tree number of edges should be equal to the number of vertices - 1
    // If less - some vertices/trees are disconnected
    // If more - some edges connect already connected vertices
    if (edges.length != n - 1) return false

    val uf = new QuickUnion(n)
    var (res, i) = (true, 0)
    while (res && i < edges.length) {
      val edge = edges(i)
      if (uf.connected(edge(0), edge(1))) res = false
      uf.union(edge(0), edge(1))
      i += 1
    }
    res
  }

  def countComponents(n: Int, edges: Array[Array[Int]]): Int = {
    val qf = new QuickFind(n)
    var res = n
    edges.foreach(e => {
      res -= (if (qf.connected(e(0), e(1))) 0 else 1)
      qf.union(e(0), e(1))
    })
    res
  }

  def earliestAcq(logs: Array[Array[Int]], n: Int): Int = {
    val sortedLogs = logs.map { case Array(x1, x2, x3) => (x1, x2, x3) }.sorted
    var (c, i, res) = (n, 0, -1)
    val qu = new QuickUnion(n)
    while (c > 1 && i < logs.length) {
      if (!qu.connected(sortedLogs(i)._2, sortedLogs(i)._3)) {
        qu.union(sortedLogs(i)._2, sortedLogs(i)._3)
        c -= 1
      }
      if (c == 1) res = sortedLogs(i)._1
      i += 1
    }
    res
  }

  def smallestStringWithSwaps(s: String, pairs: List[List[Int]]): String = {
    val res: Array[Char] = s.toArray
    val qf = new QuickFind(s.length)
    for (pair <- pairs) qf.union(pair.head, pair.last)
    val map: mutable.Map[Int, Set[(Int, Char)]] = mutable.Map()
    for (elem <- qf.root.zipWithIndex) {
      map.update(elem._1, map.getOrElse(elem._1, Set()) + ((elem._2, s(elem._2))))
    }

    for (key <- map.values) {
      val sortedByIndex = key.toArray.map(_._1).sorted
      val sortedByChar = key.toArray.map(_._2).sorted
      for (elem <- sortedByIndex.zip(sortedByChar)) res(elem._1) = elem._2
    }
    res.mkString("")

  }

  def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {

    val root: mutable.Map[String, String] = mutable.Map()
    val rank: mutable.Map[String, Int] = mutable.Map()
    val division: mutable.Map[String, Double] = mutable.Map()

    def find(x: String): String = {
      if (!root.contains(x)) {
        root(x) = x
        rank(x) = 1
        division(x) = 1
      }

      if (x == root(x)) x
      else {
        division(x) = division(x) * division(root(x))
        root(x) = find(root(x))
        root(x)
      }
    }

    def union(x: String, y: String, v: Double): Unit = {
      val rootX = find(x)
      val rootY = find(y)
      if (rootX != rootY) {
        if (rank(rootX) > rank(rootY)) {
          root(rootY) = rootX
          division(rootY) = (division(x) / v) / division(y)
        } else if (rank(rootX) < rank(rootY)) {
          root(rootX) = rootY
          division(rootX) = division(y) * v
        }
        else {
          root(rootY) = rootX
          division(rootY) = (division(x) / v) / division(y)
          rank(rootX) += 1
        }
      }
    }

    for (eq <- equations.zip(values)) {
      union(eq._1.head, eq._1.last, eq._2)
    }

    var res: Array[Double] = Array()
    for (q <- queries) {
      if (division.contains(q.head) && division.contains(q.last) && find(q.head) == find(q.last)) {
        var leftHead = q.head
        var left = division(leftHead)
        while (root(leftHead) != leftHead) {
          leftHead = root(leftHead)
          left = left * division(leftHead)
        }

        var rightHead = q.last
        var right = division(rightHead)
        while (root(rightHead) != rightHead) {
          rightHead = root(rightHead)
          right = right * division(rightHead)
        }

        res = res :+ left / right
      } else res = res :+ -1.0
    }
    res
  }

}