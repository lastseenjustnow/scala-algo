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

  def middleNode(head: ListNode): ListNode = {
    var first = head
    var second = head
    while (second.next != null) {
      first = first.next
      second = second.next
      if (second.next != null) second = second.next
    }
    first
  }

  def hasCycle(head: ListNode): Boolean = {
    if (head == null || head.next == null) return false
    var first = head
    var second = head.next
    while (second != null) {
      if (first == second) return true
      first = first.next
      second = second.next
      if (second != null) second = second.next
    }
    false
  }

  def oddEvenList(head: ListNode): ListNode = {
    if (head == null) return null
    var odd = head
    var even = head.next
    val evenHead = even
    while (even != null && even.next != null) {
      odd.next = even.next
      odd = odd.next
      even.next = odd.next
      even = even.next
    }
    odd.next = evenHead
    head
  }

  def swapPairs(head: ListNode): ListNode = {
    if (head == null || head.next == null) return head

    var previous: ListNode = null
    var current = head
    var next = head.next
    val res = next

    while (current != null && next != null) {

      // swap this pair links
      current.next = next.next
      next.next = current
      if (previous != null) previous.next = next

      // iterate
      previous = current
      current = current.next
      if (current != null) next = current.next

    }
    res
  }

  def pairSum(head: ListNode): Int = {
    var twinSum = -1
    var stack: List[Int] = List()

    var firstPointer = head
    var secondPointer = head.next

    // Add first half of the list into stack
    while (secondPointer != null) {
      stack = firstPointer.x +: stack
      firstPointer = firstPointer.next
      secondPointer = secondPointer.next
      if (secondPointer != null) secondPointer = secondPointer.next
    }

    // Update max
    while (firstPointer != null) {
      twinSum = twinSum max (firstPointer.x + stack.head)
      firstPointer = firstPointer.next
      stack = stack.tail
    }

    twinSum

  }

}