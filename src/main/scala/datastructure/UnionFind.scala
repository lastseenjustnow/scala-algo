package datastructure

class UnionFind {
  /** Disjoint set */
  var root: Array[Int] = Array()
  var rank: Array[Int] = Array()

  def this(size: Int) {
    this()
    this.root = Array.fill(size)(0)
    this.rank = Array.fill(size)(0)
    (0 until size).foreach(i => {
      root(i) = i
      rank(i) = 1
    })
  }

  def find(x: Int): Int = {
    if (x == root(x)) x
    else {
      root(x) = find(root(x))
      root(x)
    }
  }

  def union(x: Int, y: Int): Unit = {
    val rootX = find(x)
    val rootY = find(y)
    if (rootX != rootY) {
      if (rank(rootX) > rank(rootY)) root(rootY) = rootX
      else if (rank(rootX) < rank(rootY)) root(rootX) = rootY
      else {
        root(rootY) = rootX
        rank(rootX) += 1
      }
    }
  }

  def connected(x: Int, y: Int): Boolean = {
    find(x) == find(y)
  }
}
