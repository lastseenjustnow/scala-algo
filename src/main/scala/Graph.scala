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

  def leadsToDestination(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    if (n == 1) return edges.isEmpty
    var res: Boolean = true
    val adj: mutable.Map[Int, Set[Int]] = mutable.Map()
    for (n <- 0 until n) adj(n) = Set()
    edges.foreach(e => adj(e(0)) = adj(e(0)) + e(1))

    if (adj(destination).nonEmpty) res = false
    var stack: List[List[Int]] = List(List(source))
    while (res && stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      if (adj(h.head).isEmpty) res = false
      for (v <- adj(h.head) if !(v == destination)) {
        if (h.contains(v)) res = false
        stack = (v +: h) +: stack
      }
    }
    res
  }

  def levelOrder(root: Node): List[List[Int]] = {
    if (root == null) return List()
    var stack: List[Node] = List(root)
    var res: List[List[Int]] = List()

    while (stack.nonEmpty) {
      val s = stack.size
      var level: List[Int] = List()
      for (_ <- 0 until s) {
        val h = stack.head
        stack = stack.tail
        level = level :+ h.value
        stack = stack ++ h.neighbors
      }
      res = res :+ level
    }
    res
  }

  def orangesRotting(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid(0).length
    val dirs = Array((-1, 0), (0, -1), (0, 1), (1, 0))

    var stack: List[(Int, Int)] = List()
    var freshOranges = 0
    for (i <- 0 until n; j <- 0 until m) {
      if (grid(j)(i) == 1) freshOranges += 1
      else if (grid(j)(i) == 2) stack = (i, j) +: stack
    }

    def fresh(x: (Int, Int)): Array[(Int, Int)] = {
      dirs
        .map(d => (d._1 + x._1, d._2 + x._2))
        .filter { case (i, j) => i < n && i > -1 && j < m && j > -1 && grid(j)(i) == 1 }
    }

    var minutes = 0

    while (freshOranges != 0 && stack.nonEmpty) {
      val s = stack.size
      for (_ <- 0 until s) {
        val h = stack.head
        stack = stack.tail
        for (o <- fresh(h)) {
          grid(o._2)(o._1) = 2
          freshOranges -= 1
          stack = stack :+ o
        }
      }
      minutes += 1
    }

    if (freshOranges != 0) -1 else minutes
  }

  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
    /** Dijkstra's algorithm */
    var adjacencyMap: Map[Int, List[(Int, Int)]] = (for (x <- 1 to n) yield (x, List())).toMap
    times.foreach(time => adjacencyMap = adjacencyMap.updated(time(0), adjacencyMap(time(0)) :+ ((time(1), time(2)))))

    val traversed: Array[Boolean] = true +: Array.fill(n)(false)
    val traversedWeight: Array[Int] = Int.MinValue +: Array.fill(n)(Int.MaxValue)
    traversedWeight(k) = 0
    var stack: List[Int] = List(k)

    while (stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      for (e <- adjacencyMap(h)) {
        if (!traversed(e._1) || (traversedWeight(h) + e._2) < traversedWeight(e._1)) stack = stack :+ e._1
        traversedWeight(e._1) = traversedWeight(e._1) min (traversedWeight(h) + e._2)
      }
      traversed(h) = true
    }

    val res = traversedWeight.max
    if (res == Int.MaxValue) -1 else res
  }

  def minimumEffortPath(heights: Array[Array[Int]]): Int = {
    /** Breadth-first search */
    val rows = heights(0).length
    val columns = heights.length

    def neighbours(x: Int, y: Int): Array[(Int, Int)] = {
      val cc = Array((1, 0), (0, 1), (-1, 0), (0, -1))
      cc.map(c => (x + c._1, y + c._2)).filter(c => c._1 >= 0 && c._1 < rows && c._2 >= 0 && c._2 < columns)
    }

    val mx = Array.fill(columns)(Array.fill(rows)(Int.MaxValue))
    mx(0)(0) = 0
    var stack: List[(Int, Int)] = List((0, 0))

    while (stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      neighbours(h._1, h._2).foreach(nei => {
        val diff = Math.abs(heights(nei._2)(nei._1) - heights(h._2)(h._1)) max mx(h._2)(h._1)
        if (diff < mx(nei._2)(nei._1)) {
          mx(nei._2)(nei._1) = diff
          stack = stack :+ nei
        }
      })
    }
    mx(columns - 1)(rows - 1)
  }

  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    /** Kahn's algorithm */
    if (prerequisites.isEmpty) return (0 until numCourses).toArray

    var outDegree: Map[Int, List[Int]] = (0 until numCourses).map(x => (x, List[Int]())).toMap
    var inDegree: Map[Int, Int] = (0 until numCourses).map(x => (x, 0)).toMap
    prerequisites.foreach(e => {
      outDegree = outDegree.updated(e(1), outDegree(e(1)) :+ e(0))
      inDegree = inDegree.updated(e(0), inDegree(e(0)) + 1)
    })

    var q: List[Int] = inDegree.filter { case (_, value) => value == 0 }.keys.toList
    var res: List[Int] = List()

    while (q.nonEmpty) {
      val h = q.head
      q = q.tail
      res = res :+ h
      inDegree = inDegree - h
      for (v <- outDegree(h)) {
        inDegree = inDegree.updated(v, inDegree(v) - 1)
        if (inDegree(v) == 0) q = q :+ v
      }
    }

    if (res.length != numCourses) Array() else res.toArray
  }

  def alienOrder(words: Array[String]): String = {

    var outDegree: Map[Char, List[Char]] = words.flatten.distinct.map(x => (x, List[Char]())).toMap
    var inDegree: Map[Char, Int] = words.flatten.distinct.map(x => (x, 0)).toMap

    for (i <- 1 until words.length) {
      val lo: Option[(Char, Char)] = words(i - 1).zipAll(words(i), ' ', ' ').dropWhile(p => p._1 == p._2).headOption
      if (lo.nonEmpty && lo.get._2 == ' ') return ""
      if (lo.nonEmpty && lo.get._1 != ' ') {
        outDegree = outDegree.updated(lo.get._1, outDegree.getOrElse(lo.get._1, List()) :+ lo.get._2)
        inDegree = inDegree.updated(lo.get._2, inDegree.getOrElse(lo.get._2, 0) + 1)
      }
    }

    val len = inDegree.size
    var q: List[Char] = inDegree.filter { case (_, value) => value == 0 }.keys.toList
    var res: String = ""

    while (q.nonEmpty) {
      val h = q.head
      q = q.tail
      res = res + h
      inDegree = inDegree - h
      for (v <- outDegree(h)) {
        inDegree = inDegree.updated(v, inDegree(v) - 1)
        if (inDegree(v) == 0) q = q :+ v
      }
    }

    if (res.length != len) "" else res.mkString
  }

  def findMinHeightTrees(n: Int, edges: Array[Array[Int]]): List[Int] = {
    /** Idea: tree has maximum two centroids */
    if (n == 1) return List(0)

    var adj: Map[Int, Set[Int]] = (0 until n).map(x => (x, Set[Int]())).toMap
    edges.foreach(e => {
      adj = adj.updated(e(0), adj(e(0)) + e(1))
      adj = adj.updated(e(1), adj(e(1)) + e(0))
    })

    while (adj.size > 2) {
      adj.foreach(e =>
        if (e._2.size == 1) {
          adj(e._1).foreach(v =>
            adj = adj.updated(v, adj(v) - e._1)
          )
          adj = adj - e._1

        }
      )
    }
    adj.keys.toList
  }

  def minimumSemesters(n: Int, relations: Array[Array[Int]]): Int = {
    /** Kahn's algorithm */
    var outDegree: Map[Int, List[Int]] = (1 to n).map(x => (x, List[Int]())).toMap
    var inDegree: Map[Int, Int] = (1 to n).map(x => (x, 0)).toMap
    relations.foreach(e => {
      outDegree = outDegree.updated(e(0), outDegree(e(0)) :+ e(1))
      inDegree = inDegree.updated(e(1), inDegree(e(1)) + 1)
    })

    var q: List[Int] = inDegree.filter { case (_, value) => value == 0 }.keys.toList
    var sems: Int = 0

    while (q.nonEmpty) {
      sems += 1
      for (_ <- q.indices) {
        val h = q.head
        q = q.tail
        inDegree = inDegree - h
        for (v <- outDegree(h)) {
          inDegree = inDegree.updated(v, inDegree(v) - 1)
          if (inDegree(v) == 0) q = q :+ v
        }
      }
    }

    if (inDegree.nonEmpty) -1 else sems
  }

  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    var stack: List[Int] = List(0)
    val visited: Array[Boolean] = true +: Array.fill(rooms.length - 1)(false)
    while (stack.nonEmpty) {
      val head = stack.head
      stack = stack.tail
      for (key <- rooms(head) if !visited(key)) stack = stack :+ key
      visited(head) = true
    }
    if (visited.contains(false)) false else true
  }

}
