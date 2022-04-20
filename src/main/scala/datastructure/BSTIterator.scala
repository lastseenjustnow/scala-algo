package datastructure

class BSTIterator(_root: TreeNode) {

  private var nextStack: List[TreeNode] = List[TreeNode]()
  private var prevStack: List[TreeNode] = List[TreeNode]()
  private var iterateRoot = _root
  while (iterateRoot != null) {
    nextStack = iterateRoot +: nextStack
    iterateRoot = iterateRoot.left
  }

  def next(): Int = {
    val node = nextStack.head
    nextStack = nextStack.tail
    var nodeRight = node.right
    while (nodeRight != null) {
      nextStack = nodeRight +: nextStack
      nodeRight = nodeRight.left
    }
    prevStack = node +: prevStack
    node.value
  }

  def prev(): Int = {
    val node = prevStack.head
    var nodeRight = node.right
    while (nodeRight != null) {
      nextStack = nextStack.tail
      nodeRight = nodeRight.left
    }
    nextStack = node +: nextStack
    prevStack = prevStack.tail
    prevStack.head.value
  }

  def hasNext: Boolean = nextStack.nonEmpty
  def hasPrev: Boolean = prevStack.size > 1

}
