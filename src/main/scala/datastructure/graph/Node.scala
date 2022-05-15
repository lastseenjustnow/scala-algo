package datastructure.graph

class Node(var _value: Int) {
  var value: Int = _value
  var neighbors: List[Node] = List()
}
