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
}
