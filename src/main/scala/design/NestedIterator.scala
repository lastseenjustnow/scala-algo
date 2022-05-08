package design

class NestedIterator(_nestedList: List[NestedInteger]) {

  var q: List[NestedInteger] = _nestedList

  def next(): Int = {
    val h = q.head
    q = q.tail
    h.getInteger
  }

  def hasNext: Boolean = {
    while (q.nonEmpty) {
      if (q.head.isInteger) return true
      val h = q.head
      if (!h.isInteger) {
        q = q.tail
        val l = h.getList
        if (l.length > 1) q = l.tail.toList ++ q
        if (l.length > 0) q = l.head +: q
      }
    }
    false
  }
}
