import datastructure.graph.Node

import scala.collection.mutable

object Graph {
  def validPathDFS(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    /** Depth-first search */
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

  def validPathBFS(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    /** Breadth-first search */
    val adj: mutable.Map[Int, List[Int]] = mutable.Map()
    edges.foreach(e => {
      adj(e(0)) = adj.getOrElseUpdate(e(0), List()) :+ e(1)
      adj(e(1)) = adj.getOrElseUpdate(e(1), List()) :+ e(0)
    })

    var q: List[Int] = List(source)
    var found: Boolean = if (!(source == destination)) false else true
    val traversed: Array[Boolean] = Array.fill(n)(false)

    while (!found && q.nonEmpty) {
      val h = q.head
      q = q.tail
      for (e <- adj(h) if !traversed(h)) {
        if (e == destination) found = true
        q = q :+ e
      }
      traversed(h) = true
    }
    found
  }

  def allPathsSourceTargetDFS(graph: Array[Array[Int]]): List[List[Int]] = {
    /** Depth-first search */
    var res: List[List[Int]] = List()

    def rec(node: Int, path: List[Int]): Unit = {
      val p = path :+ node
      if (node == graph.length - 1) res = res :+ p
      else graph(node).foreach(e => if (!path.contains(e)) rec(e, p))
    }

    rec(0, List())
    res
  }

  def allPathsSourceTargetBFS(graph: Array[Array[Int]]): List[List[Int]] = {
    /** Breadth-first search */
    var res: List[List[Int]] = List()
    var q: List[List[Int]] = List(List(0))

    while (q.nonEmpty) {
      val h = q.head
      q = q.tail
      if (h.last == graph.length - 1) res = res :+ h
      else for (v <- graph(h.last)) q = q :+ (h :+ v)
    }
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

  def shortestPathBinaryMatrix(grid: Array[Array[Int]]): Int = {
    if (grid(0)(0) == 1) return -1
    var q: List[(Int, Int)] = List((0, 0))
    val cc = Array((1, 1), (1, 0), (0, 1), (1, -1), (-1, 1), (-1, 0), (0, -1), (-1, -1))
    var l = 0
    val n = grid.length
    val g = grid
    var found = false

    while (!found && q.nonEmpty) {
      l += 1
      var s = q.length - 1
      while (!found && s >= 0) {
        val h = q.head
        q = q.tail
        if (h == (n - 1, n - 1)) found = true
        for (c <- cc) {
          if (h._1 + c._1 >= 0 && h._1 + c._1 < n && h._2 + c._2 >= 0 && h._2 + c._2 < n && !(g(h._2 + c._2)(h._1 + c._1) == 1)) {
            q = q :+ (h._1 + c._1, h._2 + c._2)
            g(h._2 + c._2)(h._1 + c._1) = 1
          }
        }
        s -= 1
      }
    }
    if (!found) -1 else l
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
