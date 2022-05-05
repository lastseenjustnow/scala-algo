package datastructure.graph

class QuickFind(size: Int) extends DisjointSet(size) {
  override def find(x: Int): Int = root(x)

  override def union(x: Int, y: Int): Unit = {
    val rootX = find(x)
    val rootY = find(y)
    if (rootX != rootY) {
      root.indices.foreach(
        i => if (root(i) == rootY) root(i) = rootX
      )
    }
  }
}
