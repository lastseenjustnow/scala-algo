import datastructure.TreeNode

import scala.collection.mutable

object Trees {

  def preorderTraversalRecursive(root: TreeNode): List[Int] = {
    def rec(node: TreeNode, lst: List[Int]): List[Int] = {
      node match {
        case null => lst
        case _ =>
          val leftList = rec(node.left, lst :+ node.value)
          rec(node.right, leftList)
      }
    }

    rec(root, List())
  }

  def preorderTraversalIterative(root: TreeNode): List[Int] = {
    var stack: List[TreeNode] = List(root)
    var res: List[Int] = List()

    while (stack.nonEmpty) {
      var pop = stack.last
      stack = stack.drop(1)
      while (pop != null) {
        res = res :+ pop.value
        if (pop.right != null) stack = stack :+ pop.right
        pop = pop.left
      }
    }
    res
  }

  def inorderTraversalRecursive(root: TreeNode): List[Int] = {
    def rec(node: TreeNode, lst: List[Int]): List[Int] = {
      node match {
        case null => lst
        case _ =>
          val leftList = rec(node.left, lst)
          rec(node.right, leftList :+ node.value)
      }
    }

    rec(root, List())
  }

  def inorderTraversalIterative(root: TreeNode): List[Int] = {
    var stack: List[TreeNode] = List()
    var res: List[Int] = List()
    var node = root
    while (node != null) {
      stack = node +: stack
      node = node.left
    }

    while (stack.nonEmpty) {
      res = res :+ stack.head.value
      node = stack.head.right
      stack = stack.tail
      while (node != null) {
        stack = node +: stack
        node = node.left
      }
    }
    res
  }

  def postorderTraversalRecursive(root: TreeNode): List[Int] = {
    def rec(node: TreeNode, lst: List[Int]): List[Int] = {
      node match {
        case null => lst
        case _ =>
          val leftList = rec(node.left, lst)
          rec(node.right, leftList) :+ node.value
      }
    }

    rec(root, List())
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) return List()
    val q: mutable.Queue[TreeNode] = mutable.Queue(root, null)
    var res: List[List[Int]] = List()
    var level: List[Int] = List()

    while (q.size > 1) {
      val node = q.dequeue()
      if (node == null) {
        res = res :+ level
        level = List()
        q.enqueue(null)
      } else {
        level = level :+ node.value
        if (node.left != null) q.enqueue(node.left)
        if (node.right != null) q.enqueue(node.right)
      }
    }
    res :+ level
  }


  def searchBST(root: TreeNode, `val`: Int): TreeNode = {
    root match {
      case null => null
      case x if x.value == `val` => root
      case _ =>
        val leftSubtree = searchBST(root.left, `val`)
        if (leftSubtree != null) leftSubtree else searchBST(root.right, `val`)
    }
  }

  def trimBST(root: TreeNode, low: Int, high: Int): TreeNode = {
    def rec(currentNode: TreeNode): TreeNode = {
      currentNode match {
        case null => null
        case x if x.value > high => rec(currentNode.left)
        case x if x.value < low => rec(currentNode.right)
        case _ =>
          currentNode.left = rec(currentNode.left)
          currentNode.right = rec(currentNode.right)
          currentNode
      }
    }

    rec(root)
  }

  def convertBST(root: TreeNode): TreeNode = {
    val stack = mutable.Stack[TreeNode]()
    var thisNode = root
    var cumsum = 0

    while (thisNode != null) {
      stack.push(thisNode)
      thisNode = thisNode.right
    }

    while (stack.nonEmpty) {
      thisNode = stack.pop()
      cumsum += thisNode.value
      thisNode.value = cumsum
      var leftSubNode = thisNode.left
      while (leftSubNode != null) {
        stack.push(leftSubNode)
        leftSubNode = leftSubNode.right
      }
    }
    root
  }

  def increasingBST(root: TreeNode): TreeNode = {
    val stack = mutable.Stack[TreeNode]()
    var newRoot = root
    while (newRoot != null) {
      stack.push(newRoot)
      newRoot = newRoot.right
    }

    while (stack.nonEmpty) {
      var pop = stack.pop()
      pop.right = newRoot
      newRoot = pop
      pop = pop.left
      while (pop != null) {
        stack.push(pop)
        pop = pop.right
      }
      newRoot.left = null
    }
    newRoot
  }

  def kthSmallest(root: TreeNode, k: Int): Int = {
    val stack = mutable.Stack[TreeNode]()
    var node = root
    while (node != null) {
      stack.push(node)
      node = node.left
    }
    var (i, res) = (0, 0)

    while (i < k) {
      node = stack.pop()
      res = node.value
      node = node.right
      while (node != null) {
        stack.push(node)
        node = node.left
      }
      i += 1
    }
    res
  }

  def recoverTree(root: TreeNode): Unit = {
    val stack = mutable.Stack[TreeNode]()

    var (leftToSwap, rightToSwap) = (root, root)

    var flag = false
    var node = root
    while (node != null) {
      stack.push(node)
      node = node.left
    }

    while (!flag) {
      node = stack.pop()
      val thisNode = node
      node = node.right
      while (node != null) {
        stack.push(node)
        node = node.left
      }
      if (thisNode.value > stack.head.value) {
        flag = true
        leftToSwap = thisNode
      }
    }

    flag = false
    node = root
    while (node != null) {
      stack.push(node)
      node = node.right
    }

    while (!flag) {
      node = stack.pop()
      val thisNode = node
      node = node.left
      while (node != null) {
        stack.push(node)
        node = node.right
      }
      if (thisNode.value < stack.head.value) {
        flag = true
        rightToSwap = thisNode
      }
    }

    val tempValue = leftToSwap.value
    leftToSwap.value = rightToSwap.value
    rightToSwap.value = tempValue
  }

}
