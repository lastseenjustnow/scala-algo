package datastructure.graph

class QuickUnion(size: Int) extends DisjointSet(size) {

  override def find(x: Int): Int = {
    if (x == root(x)) x
    else {
      root(x) = find(root(x))
      root(x)
    }
  }

  override def union(x: Int, y: Int): Unit = {
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
}

