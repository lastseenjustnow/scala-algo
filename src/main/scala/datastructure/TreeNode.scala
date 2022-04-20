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

    var parentalIndex = 0
    for (i <- 1 until arr.length by 2) {
      val leftNode = arrNodes(i)
      val rightNode = if (i + 1 < arr.length) arrNodes(i + 1) else null
      while (arr(parentalIndex) == null) parentalIndex += 1
      arrNodes(parentalIndex).left = leftNode
      arrNodes(parentalIndex).right = rightNode
      parentalIndex += 1
    }
    if (arr.length > 0) arrNodes(0) else null
  }
}
