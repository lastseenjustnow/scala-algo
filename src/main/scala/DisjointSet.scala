import datastructure.UnionFind

object DisjointSet {
  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    val n = isConnected.length
    var res = n
    val uf = new UnionFind(n)

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
}
