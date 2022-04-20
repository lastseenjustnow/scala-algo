import org.scalatest.FunSuite
import datastructure.{BSTIterator, TreeNode}
import Trees._

class TreesTest extends FunSuite {

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

}
