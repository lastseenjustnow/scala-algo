package datastructure.graph

abstract class DisjointSet(size: Int) {

  var root: Array[Int] = Array.fill(size)(0)
  var rank: Array[Int] = Array.fill(size)(0)

  (0 until size).foreach(i => {
    root(i) = i
    rank(i) = 1
  })


  def find(x: Int): Int

  def union(x: Int, y: Int): Unit

  def connected(x: Int, y: Int): Boolean = find(x) == find(y)
}
