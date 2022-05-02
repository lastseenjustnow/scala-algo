package datastructure

class TreeNode(_value: Int = 0,
               _left: TreeNode = null,
               _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
  var next: TreeNode = null

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

  def toArray: Array[Any] = {
    var stack: List[TreeNode] = List(this)
    var counter: Int = 1
    var res: Array[Any] = Array()

    while (counter > 0) {
      val node = stack.head
      stack = stack.tail
      if (node != null) {
        res = res :+ node.value
        counter -= 1
        stack = stack :+ node.left
        if (node.left != null) counter += 1
        stack = stack :+ node.right
        if (node.right != null) counter += 1
      } else {
        res = res :+ null
      }
    }
    res
  }

  def serialize: String = {
    var stack: List[TreeNode] = List(this)
    var counter: Int = 1
    var res: Array[String] = Array()

    while (counter > 0) {
      val node = stack.head
      stack = stack.tail
      if (node != null) {
        res = res :+ node.value.toString
        counter -= 1
        stack = stack :+ node.left
        if (node.left != null) counter += 1
        stack = stack :+ node.right
        if (node.right != null) counter += 1
      } else {
        res = res :+ "_"
      }
    }
    res.mkString(",")
  }

  def deserialize(data: String): TreeNode = {

    val dataSplit = data.split(",")
    val arrNodes = dataSplit.map {
      case x if x.head == '_' => null
      case someVal => new TreeNode(someVal.toInt)
    }

    var parentalIndex = 0
    for (i <- 1 until dataSplit.length by 2) {
      val leftNode = arrNodes(i)
      val rightNode = if (i + 1 < dataSplit.length) arrNodes(i + 1) else null
      while (dataSplit(parentalIndex) == "_") parentalIndex += 1
      arrNodes(parentalIndex).left = leftNode
      arrNodes(parentalIndex).right = rightNode
      parentalIndex += 1
    }
    if (data.nonEmpty) arrNodes(0) else null
  }

}
