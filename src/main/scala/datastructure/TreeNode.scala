package datastructure

class TreeNode(_value: Int = 0,
               _left: TreeNode = null,
               _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right

  def fromArray(arr: Array[Any]): TreeNode = {
    val arrNodes = arr.map {
      case null => null
      case someVal: Int => new TreeNode(someVal)
    }

    for (i <- 1 until arr.length) {
      val nodeIndex = (i - 1) / 2
      val leftOrRight = (i - 1) % 2
      leftOrRight match {
        case x if x == 0 => arrNodes(nodeIndex).left = arrNodes(i)
        case x if x == 1 => arrNodes(nodeIndex).right = arrNodes(i)
      }
    }
    arrNodes(0)
  }
}
