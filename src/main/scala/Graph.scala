import datastructure.graph.Node

import scala.collection.mutable

object Graph {
  def validPath(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    val traversed: Array[Boolean] = Array.fill(n)(false)
    var stack: List[Int] = List(source)
    var res = if (!(source == destination)) false else true

    while (!res && stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      edges.foreach(e => {
        if (e(0) == h && !traversed(e(1))) stack = stack :+ e(1)
        else if (e(1) == h && !traversed(e(0))) stack = stack :+ e(0)
        if (e(0) == h && e(1) == destination || e(1) == h && e(0) == destination) res = true
      }
      )
      traversed(h) = true
    }
    res

  }

  def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
    var res: List[List[Int]] = List()

    def rec(node: Int, path: List[Int]): Unit = {
      val p = path :+ node
      if (node == graph.length - 1) res = res :+ p
      else graph(node).foreach(e => if (!path.contains(e)) rec(e, p))
    }

    rec(0, List())
    res
  }

  def cloneGraph(graph: Node): Node = {
    if (graph == null) return null
    val map: mutable.Map[Int, Node] = mutable.Map()
    var traversed: Set[Int] = Set()

    var stack: List[Node] = List(graph)
    val deepHead = new Node(graph.value)
    map += (graph.value -> deepHead)

    while (stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      traversed = traversed + h.value
      h.neighbors.foreach(n => {
        val newH = map.getOrElseUpdate(h.value, new Node(h.value))
        val newN = map.getOrElseUpdate(n.value, new Node(n.value))
        newH.neighbors = newH.neighbors :+ newN
        if (!traversed.contains(n.value)) {
          stack = n +: stack
          traversed = traversed + n.value
        }
      })
    }
    deepHead
  }

  def findItinerary(tickets: List[List[String]]): List[String] = {
    val map: mutable.Map[String, mutable.SortedMap[String, Int]] = mutable.Map()
    var tc = 0
    tickets.foreach {
      t => {
        val tr = map.getOrElseUpdate(t.head, mutable.SortedMap[String, Int]())
        tr(t.last) = tr.getOrElse(t.last, 0) + 1
        tc += 1
      }
    }

    var resString: String = ""

    def backtrack(node: String, path: String, i: Int): Unit = {
      if (path.length == tc * 3 + 3) resString = path
      else if (map.contains(node)) {
        for (n <- map(node)) {
          if (n._2 != 0 && resString.isEmpty) {
            map(node)(n._1) -= 1
            backtrack(n._1, path + n._1, i + 3)
            map(node)(n._1) += 1
          }
        }
      }
    }

    backtrack("JFK", "JFK", 3)
    (0 until resString.length by 3).map(i => resString.slice(i, i + 3)).toList
  }
}
