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

  def maxDepth(root: TreeNode): Int = {
    def rec(node: TreeNode, res: Int): Int = {
      node match {
        case null => res - 1
        case _ => rec(node.left, res + 1) max rec(node.right, res + 1)
      }
    }

    rec(root, 1)
  }

  def isSymmetricRecursive(root: TreeNode): Boolean = {
    def compareNodes(leftNode: TreeNode, rightNode: TreeNode): Boolean = {
      if (leftNode == null || rightNode == null) leftNode == rightNode
      else if (leftNode.value != rightNode.value) false
      else compareNodes(leftNode.left, rightNode.right) && compareNodes(leftNode.right, rightNode.left)
    }

    compareNodes(root.left, root.right)
  }

  def isSymmetricIterative(root: TreeNode): Boolean = {
    var stackOne: List[TreeNode] = List(root.left)
    var stackTwo: List[TreeNode] = List(root.right)
    var res: Boolean = true

    while (res && stackOne.nonEmpty) {
      val leftNode = stackOne.head
      stackOne = stackOne.tail
      val rightNode = stackTwo.head
      stackTwo = stackTwo.tail
      if (leftNode == null || rightNode == null) res = leftNode == rightNode
      else {
        res = leftNode.value == rightNode.value
        stackOne = leftNode.left +: stackOne
        stackTwo = rightNode.right +: stackTwo
        stackOne = leftNode.right +: stackOne
        stackTwo = rightNode.left +: stackTwo
      }
    }
    res
  }

  def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {
    if (root == null) return false

    def rec(node: TreeNode, curSum: Int): Boolean = {
      (node.left, node.right) match {
        case (null, null) => targetSum == curSum + node.value
        case (_, null) => rec(node.left, node.value + curSum)
        case (null, _) => rec(node.right, node.value + curSum)
        case _ => rec(node.left, node.value + curSum) || rec(node.right, node.value + curSum)
      }
    }

    rec(root, 0)
  }

  def countUnivalSubtrees(root: TreeNode): Int = {
    if (root == null) return 0

    def rec(node: TreeNode): (Int, Set[Int]) = {
      node match {
        case null => (0, Set[Int]())
        case _ =>
          val leftNode = rec(node.left)
          val rightNode = rec(node.right)
          val unioned = leftNode._2 union rightNode._2 union Set(node.value)
          (leftNode._1 + rightNode._1 + (if (unioned.size == 1) 1 else 0), unioned)
      }
    }

    rec(root)._1
  }

  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
    inorder.length match {
      case x if x == 0 => null
      case x if x == 1 => new TreeNode(inorder.head)
      case _ =>
        val rootVal = postorder.last
        val i = inorder.indexWhere(_ == rootVal)
        val leftNode = buildTree(inorder.take(i), postorder.take(i))
        val rightNode = buildTree(inorder.drop(i + 1), postorder.drop(i).dropRight(1))
        new TreeNode(rootVal, leftNode, rightNode)
    }
  }

  def buildTreePreorderInorder(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    val map = inorder.zipWithIndex.map(x => x._1 -> x._2).toMap
    var i = 0

    def rec(left: Int, right: Int): TreeNode = {
      if (left > right) null
      else {
        val rootVal = preorder(i)
        i += 1
        val leftNode = rec(left, map(rootVal) - 1)
        val rightNode = rec(map(rootVal) + 1, right)
        new TreeNode(rootVal, leftNode, rightNode)
      }
    }

    rec(0, inorder.length - 1)
  }

  def rightSideView(root: TreeNode): List[Int] = {
    if (root == null) return List()
    var res: List[Int] = List()
    var q: List[TreeNode] = List(root)

    while (q.nonEmpty) {
      val s = q.length
      res = res :+ q.head.value
      for (_ <- 0 until s) {
        val h = q.head
        q = q.tail
        if (h.right != null) q = q :+ h.right
        if (h.left != null) q = q :+ h.left
      }
    }
    res
  }

  def connectNaive(root: TreeNode): TreeNode = {
    var stack: List[TreeNode] = List(root, null)

    while (stack.nonEmpty) {
      val node = stack.head
      stack = stack.tail
      if (node != null) {
        node.next = stack.head
        if (node.left != null) {
          stack = stack :+ node.left
          stack = stack :+ node.right
        }
      } else if (stack.length > 1) stack = stack :+ node
    }
    root
  }

  def connect2(root: TreeNode): TreeNode = {
    if (root == null) return null
    var stack: List[TreeNode] = List(root)

    while (stack.nonEmpty) {
      val size = stack.size
      for (i <- 0 until size) {
        val node = stack.head
        stack = stack.tail
        if (i != size - 1) node.next = stack.head
        if (node.left != null) stack = stack :+ node.left
        if (node.right != null) stack = stack :+ node.right
      }
    }
    root
  }

  def connect2Optimized(root: TreeNode): TreeNode = {
    var leftmost = root

    while (leftmost != null) {
      var head = leftmost
      leftmost = null
      var prev: TreeNode = null
      while (head != null) {

        if (head.left != null) {
          if (leftmost == null) {
            leftmost = head.left
            prev = head.left
          } else {
            prev.next = head.left
            prev = prev.next
          }
        }


        if (head.right != null) {
          if (leftmost == null) {
            leftmost = head.right
            prev = head.right
          } else {
            prev.next = head.right
            prev = prev.next
          }
        }
        head = head.next
      }
    }

    root
  }

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    def rec(node: TreeNode, found: Int): (TreeNode, Int) = {
      (node, found) match {
        case (null, _) => (null, 0)
        case (n, f) if f == 2 => (n, f)
        case (n, _) =>
          val left = rec(n.left, found)
          val right = rec(n.right, found)
          val matched = (if (n.value == p.value || n.value == q.value) 1 else 0) + left._2 + right._2
          if (left._2 == 2) left
          else if (right._2 == 2) right
          else (n, matched)
      }
    }

    rec(root, 0)._1
  }

  def averageOfSubtree(root: TreeNode): Int = {
    def rec(node: TreeNode): (Int, Int, Int) = {
      node match {
        case null => (0, 0, 0)
        case x =>
          val left = rec(x.left)
          val right = rec(x.right)
          val thisSum = left._1 + right._1 + x.value
          val thisCount = left._2 + right._2 + 1
          val isNeededNode = if ((thisSum / thisCount) == x.value) 1 else 0
          (thisSum, thisCount, left._3 + right._3 + isNeededNode)
      }
    }

    rec(root)._3
  }

  def deepestLeavesSum(root: TreeNode): Int = {
    var q: List[TreeNode] = List(root)
    var res = 0
    while (q.nonEmpty) {
      res = 0
      val s = q.length
      for (_ <- 0 until s) {
        val h = q.head
        q = q.tail
        res += h.value
        if (h.left != null) q = q :+ h.left
        if (h.right != null) q = q :+ h.right
      }
    }
    res
  }

}
