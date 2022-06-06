import LinkedLists._
import datastructure.ListNode
import design.MyLinkedList
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

  test("Design Linked List") {
    val myLinkedList = new MyLinkedList()
    myLinkedList.addAtHead(1)
    assert(myLinkedList.get(0) == 1)
    myLinkedList.addAtTail(3)
    assert(myLinkedList.get(0) == 1)
    assert(myLinkedList.get(1) == 3)
    myLinkedList.addAtIndex(1, 2)
    assert(myLinkedList.get(0) == 1) // linked list becomes 1->2->3
    assert(myLinkedList.get(1) == 2) // linked list becomes 1->2->3
    assert(myLinkedList.get(2) == 3) // linked list becomes 1->2->3
    assert(myLinkedList.get(1) == 2) // return 2

    myLinkedList.deleteAtIndex(1)
    assert(myLinkedList.get(1) == 3) // now the linked list is 1->3

    val myLinkedList2 = new MyLinkedList()
    myLinkedList2.addAtHead(1)
    myLinkedList2.deleteAtIndex(0)

    val myLinkedList3 = new MyLinkedList()
    myLinkedList3.addAtIndex(0, 10)
    myLinkedList3.addAtIndex(0, 20)
    myLinkedList3.addAtIndex(1, 30)
    assert(myLinkedList3.get(0) == 20)

    val myLinkedList4 = new MyLinkedList()
    myLinkedList4.addAtTail(1)
    assert(myLinkedList4.get(0) == 1)

    val myLinkedList5 = new MyLinkedList()
    myLinkedList5.addAtHead(2)
    myLinkedList5.deleteAtIndex(1)
    myLinkedList5.addAtHead(2)
    myLinkedList5.addAtHead(7)
    myLinkedList5.addAtHead(3)
    myLinkedList5.addAtHead(2)
    myLinkedList5.addAtHead(5)
    myLinkedList5.addAtTail(5)
    assert(myLinkedList5.get(5) == 2)
    myLinkedList5.deleteAtIndex(6)
    myLinkedList5.deleteAtIndex(4)

    val myLinkedList6 = new MyLinkedList()
    myLinkedList6.addAtHead(4)
    assert(myLinkedList6.get(1) == -1)
    myLinkedList6.addAtHead(1)
    myLinkedList6.addAtHead(5)
    myLinkedList6.deleteAtIndex(3)
    myLinkedList6.addAtHead(7)
    assert(myLinkedList6.get(3) == 4)
    myLinkedList6.addAtHead(1)
    myLinkedList6.deleteAtIndex(4)

    val myLinkedList7 = new MyLinkedList()
    myLinkedList7.addAtHead(1)
    myLinkedList7.addAtTail(3)
    myLinkedList7.addAtIndex(1, 2)
    assert(myLinkedList7.get(1) == 2)
    myLinkedList7.deleteAtIndex(1)
    assert(myLinkedList7.get(1) == 3)
    assert(myLinkedList7.get(3) == -1)
    myLinkedList7.deleteAtIndex(3)
    myLinkedList7.deleteAtIndex(0)
    assert(myLinkedList7.get(0) == 3)
    myLinkedList7.deleteAtIndex(0)
    assert(myLinkedList7.get(0) == -1)

    val myLinkedList8 = new MyLinkedList()
    myLinkedList8.addAtIndex(1, 0)
    assert(myLinkedList8.get(0) == -1)

  }

}
