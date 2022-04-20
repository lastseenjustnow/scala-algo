package datastructure

class BSTIterator(_root: TreeNode) {

  private var stack: List[TreeNode] = List[TreeNode]()
  private var iterateRoot = _root
  while (iterateRoot != null) {
    stack = iterateRoot +: stack
    iterateRoot = iterateRoot.left
  }

  def next(): Int = {
    val node = stack.head
    stack = stack.tail
    var nodeRight = node.right
    while (nodeRight != null) {
      stack = nodeRight +: stack
      nodeRight = nodeRight.left
    }
    node.value
  }

  def hasNext: Boolean = stack.nonEmpty

}
