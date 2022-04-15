import datastructure.TreeNode

object Trees {
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
}
