import LinkedLists._
import datastructure.ListNode
import org.scalatest.FunSuite

class LinkedListsTest extends FunSuite {
  test("Swapping Nodes in a Linked List") {
    val linkedList1 = new ListNode(1, null)
    linkedList1.next = new ListNode(2, null)
    linkedList1.next.next = new ListNode(3, null)
    linkedList1.next.next.next = new ListNode(4, null)
    linkedList1.next.next.next.next = new ListNode(5, null)
    linkedList1.next.next.next.next.next = new ListNode(6, null)

    assert(swapNodes(linkedList1, 2).toArray sameElements Array(1, 5, 3, 4, 2, 6))
    assert(swapNodes(linkedList1, 1).toArray sameElements Array(6, 5, 3, 4, 2, 1))

    val linkedList2 = new ListNode(1, null)
    linkedList2.next = new ListNode(2, null)
    linkedList2.next.next = new ListNode(3, null)

    assert(swapNodes(linkedList2, 2).toArray sameElements Array(1, 2, 3))

    val linkedList3 = new ListNode(1, null)
    linkedList3.next = new ListNode(2, null)

    assert(swapNodes(linkedList3, 2).toArray sameElements Array(2, 1))

    val linkedList4 = new ListNode(1, null)

    assert(swapNodes(linkedList4, 1).toArray sameElements Array(1))

  }
}
