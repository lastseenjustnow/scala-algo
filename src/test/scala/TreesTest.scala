import org.scalatest.FunSuite

import datastructure.TreeNode
import Trees._

class TreesTest extends FunSuite {
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
}
