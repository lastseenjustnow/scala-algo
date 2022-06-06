import datastructure.ListNode

object LinkedLists {
  def swapNodes(head: ListNode, k: Int): ListNode = {
    var currentNode = head
    var n = 1

    while (currentNode.next != null) {
      currentNode = currentNode.next
      n += 1
    }

    currentNode = head
    var (storedValue, i) = (-1, 1)

    val (firstToSwapIndex, secondToSwapIndex) = (k min (n - k) + 1, k max (n - k) + 1)
    if (firstToSwapIndex == secondToSwapIndex) return head

    while (i < secondToSwapIndex) {
      if (i == firstToSwapIndex) {
        storedValue = currentNode.x
      }
      currentNode = currentNode.next
      i += 1
    }

    val temp = currentNode.x
    currentNode.x = storedValue
    storedValue = temp

    i = 1
    currentNode = head
    while (i != firstToSwapIndex) {
      currentNode = currentNode.next
      i += 1
    }

    currentNode.x = storedValue
    head

  }

  def reverseList(head: ListNode): ListNode = {
    var prev: ListNode = null
    var curr = head
    while (curr != null) {
      val tempCurr = curr.next
      curr.next = prev
      prev = curr
      curr = tempCurr
    }
    prev
  }

}