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
}
