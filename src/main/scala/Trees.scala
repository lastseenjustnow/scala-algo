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
}
