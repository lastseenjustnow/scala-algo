import org.scalatest.FunSuite
import datastructure.{BSTIterator, TreeNode}
import Trees._

class TreesTest extends FunSuite {

  test("Create tree from Array") {
    val arr1 = Array(5, 1, 4, null, null, 3, 6)
    new TreeNode().fromArray(arr1)
  }

  test("Binary Tree Preorder Traversal") {
    val root1 = new TreeNode(1, null, new TreeNode(2, new TreeNode(3)))
    val root2 = new TreeNode(3, new TreeNode(1), new TreeNode(2))
    val root3 = new TreeNode(2, new TreeNode(1, new TreeNode(4)), new TreeNode(3))

    val conditions: Array[(TreeNode, List[Int])] = Array(
      (root1, List(1, 2, 3)),
      (root2, List(3, 1, 2)),
      (root3, List(2, 1, 4, 3))
    )

    for (cond <- conditions) {
      assert(preorderTraversalRecursive(cond._1) == cond._2)
      assert(preorderTraversalIterative(cond._1) == cond._2)
    }
  }

  test("Binary Tree Inorder Traversal") {
    val root1 = new TreeNode().fromArray(Array(1, null, 2, 3))
    val root2 = new TreeNode().fromArray(Array())
    val root3 = new TreeNode().fromArray(Array(1))


    val conditions: Array[(TreeNode, List[Int])] = Array(
      (root1, List(1, 3, 2)),
      (root2, List()),
      (root3, List(1)),
    )

    for (cond <- conditions) {
      assert(inorderTraversalRecursive(cond._1) == cond._2)
      assert(inorderTraversalIterative(cond._1) == cond._2)
    }
  }

  test("Binary Tree Postorder Traversal") {
    val root1 = new TreeNode().fromArray(Array(1, null, 2, 3))
    val root2 = new TreeNode().fromArray(Array())
    val root3 = new TreeNode().fromArray(Array(1))


    val conditions: Array[(TreeNode, List[Int])] = Array(
      (root1, List(3, 2, 1)),
      (root2, List()),
      (root3, List(1)),
    )

    for (cond <- conditions) {
      assert(postorderTraversalRecursive(cond._1) == cond._2)
    }
  }

  test("Binary Tree Level Order Traversal") {
    val root1 = new TreeNode().fromArray(Array(3, 9, 20, null, null, 15, 7))
    val root2 = new TreeNode().fromArray(Array())

    val conditions: Array[(TreeNode, List[List[Int]])] = Array(
      (root1, List(List(3), List(9, 20), List(15, 7))),
      (root2, List())
    )

    for (cond <- conditions) {
      assert(levelOrder(cond._1) == cond._2)
    }
  }

  test("Trim a Binary Search Tree") {

    val root1 = new TreeNode(1, new TreeNode(0), new TreeNode(2))
    trimBST(root1, 1, 2)
    assert(root1.value == 1)
    assert(root1.left == null)
    assert(root1.right.value == 2)

    val root2 = new TreeNode(3,
      new TreeNode(0,
        null,
        new TreeNode(2,
          new TreeNode(1),
          null
        )),
      new TreeNode(4))
    trimBST(root2, 1, 3)

    assert(root2.value == 3)
    assert(root2.left.value == 2)
    assert(root2.right == null)
    assert(root2.left.left.value == 1)
    assert(root2.left.right == null)

  }

  test("Convert BST to Greater Tree") {

    val root1 = new TreeNode(4,
      new TreeNode(1,
        new TreeNode(0),
        new TreeNode(2, null, new TreeNode(3))),
      new TreeNode(7,
        new TreeNode(5, null, new TreeNode(6)),
        new TreeNode(8, null, new TreeNode(9)))
    )
    convertBST(root1)

  }

  test("Increasing Order Search Tree") {

    val root1 = new TreeNode(5,
      new TreeNode(3,
        new TreeNode(2,
          new TreeNode(1)),
        new TreeNode(4)),
      new TreeNode(6,
        null,
        new TreeNode(8,
          new TreeNode(7),
          new TreeNode(9)))
    )
    increasingBST(root1)

    val root2 = new TreeNode(2,
      new TreeNode(1),
      new TreeNode(4, new TreeNode(3)
      ),
    )
    increasingBST(root2)

  }

  test("Recover Binary Search Tree") {

    val root1 = new TreeNode(7,
      new TreeNode(3,
        null,
        new TreeNode(4)),
      new TreeNode(8,
        new TreeNode(6))
    )
    recoverTree(root1)

  }

  test("Binary Search Tree Iterator") {

    val root1 = new TreeNode(7,
      new TreeNode(3),
      new TreeNode(15,
        new TreeNode(9),
        new TreeNode(20))
    )

    val iterator = new BSTIterator(root1)
    assert(iterator.next() == 3)
    assert(iterator.next() == 7)
    assert(iterator.next() == 9)
    assert(iterator.next() == 15)
    assert(iterator.next() == 20)
    assert(!iterator.hasNext)
  }

  test("Binary Search Tree Iterator II") {

    val root1 = new TreeNode(7,
      new TreeNode(3),
      new TreeNode(15,
        new TreeNode(9),
        new TreeNode(20))
    )

    val iterator = new BSTIterator(root1)
    assert(iterator.next() == 3)
    assert(iterator.next() == 7)
    assert(iterator.prev() == 3)
    assert(iterator.next() == 7)
    assert(iterator.hasNext)
    assert(iterator.next() == 9)
    assert(iterator.next() == 15)
    assert(iterator.next() == 20)
    assert(!iterator.hasNext)
    assert(iterator.hasPrev)
    assert(iterator.prev() == 15)
    assert(iterator.prev() == 9)

    val root2 = new TreeNode(1)
    val iterator2 = new BSTIterator(root2)
    assert(!iterator2.hasPrev)
    assert(iterator2.hasNext)
    assert(iterator2.next() == 1)
    assert(!iterator2.hasPrev)
    assert(!iterator2.hasNext)
  }

  test("Symmetric Tree") {
    val root1 = new TreeNode().fromArray(Array(1, 2, 2, 3, 4, 4, 3))
    val root2 = new TreeNode().fromArray(Array(1, 2, 2, null, 3, null, 3))


    val conditions: Array[(TreeNode, Boolean)] = Array(
      (root1, true),
      (root2, false),
    )

    for (cond <- conditions) {
      assert(isSymmetricRecursive(cond._1) == cond._2)
      assert(isSymmetricIterative(cond._1) == cond._2)
    }

  }

  test("Path Sum") {
    val root1 = new TreeNode().fromArray(Array(5, 4, 8, 11, null, 13, 4, 7, 2, null, null, null, 1))
    val root2 = new TreeNode().fromArray(Array(1, 2, 3))
    val root3 = new TreeNode().fromArray(Array())
    val root4 = new TreeNode().fromArray(Array(1, 2))
    val root5 = new TreeNode().fromArray(Array(1))


    val conditions: Array[(TreeNode, Int, Boolean)] = Array(
      (root1, 22, true),
      (root2, 5, false),
      (root3, 0, false),
      (root4, 1, false),
      (root5, 1, true)
    )

    for (cond <- conditions) {
      assert(hasPathSum(cond._1, cond._2) == cond._3)
    }
  }

  test("Count Univalue Subtrees") {
    val root1 = new TreeNode().fromArray(Array(5, 1, 5, 5, 5, null, 5))
    val root2 = new TreeNode().fromArray(Array())
    val root3 = new TreeNode().fromArray(Array(5, 5, 5, 5, 5, null, 5))
    val root4 = new TreeNode().fromArray(Array(1, 1, 1, 5, 5, null, 5))


    val conditions: Array[(TreeNode, Int)] = Array(
      (root1, 4),
      (root2, 0),
      (root3, 6),
      (root4, 3)
    )

    for (cond <- conditions) {
      assert(countUnivalSubtrees(cond._1) == cond._2)
    }
  }

  test("Construct Binary Tree from Inorder and Postorder Traversal") {

    val conditions: Array[(Array[Int], Array[Int], TreeNode)] = Array(
      (Array(16, 9, 3, 19, 4, 6), Array(16, 9, 19, 6, 4, 3), new TreeNode().fromArray(Array(3, 9, 4, 16, null, 19, 6))),
      (Array(9, 3, 15, 20, 7), Array(9, 15, 7, 20, 3), new TreeNode().fromArray(Array(3, 9, 20, null, null, 15, 7))),
      (Array(-1), Array(-1), new TreeNode().fromArray(Array(-1)))
    )

    for (cond <- conditions) {
      assert(buildTree(cond._1, cond._2).toArray.toList == cond._3.toArray.toList)
    }
  }

  test("Construct Binary Tree from Preorder and Inorder Traversal") {

    val conditions: Array[(Array[Int], Array[Int], TreeNode)] = Array(
      (Array(3, 9, 20, 15, 7), Array(9, 3, 15, 20, 7), new TreeNode().fromArray(Array(3, 9, 20, null, null, 15, 7))),
      (Array(-1), Array(-1), new TreeNode().fromArray(Array(-1)))
    )

    for (cond <- conditions) {
      assert(buildTreePreorderInorder(cond._1, cond._2).toArray.toList == cond._3.toArray.toList)
    }
  }

  test("Populating Next Right Pointers in Each Node") {

    val node1 = new TreeNode().fromArray(Array(1, 2, 3, 4, 5, 6, 7))
    connectNaive(node1)
    assert(node1.next == null)
    assert(node1.left.next.value == 3)
    assert(node1.right.next == null)
    assert(node1.left.left.next.value == 5)
    assert(node1.left.right.next.value == 6)
    assert(node1.right.left.next.value == 7)
    assert(node1.right.right.next == null)

  }
}
