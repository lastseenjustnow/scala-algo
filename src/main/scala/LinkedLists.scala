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

  def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
    def findLLlength(ll: ListNode): Int = {
      var h = ll
      var i = 0
      while (h != null) {
        h = h.next
        i += 1
      }
      i
    }

    val n = findLLlength(headA)
    val m = findLLlength(headB)
    var ah = headA
    var bh = headB

    if (n > m) {
      for (_ <- 0 until n - m) ah = ah.next
    } else {
      for (_ <- 0 until m - n) bh = bh.next
    }

    for (_ <- 0 until (m min n)) {
      if (ah == bh) return ah
      ah = ah.next
      bh = bh.next
    }
    null
  }

}