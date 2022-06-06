package design

class MyLinkedList() {

  var l: SinglyLinkedList = null

  def get(index: Int): Int = {
    if (l == null) -1 else {
      var h = l
      for (_ <- 0 until index) {
        if (h.next == null) return -1
        h = h.next
      }
      h.`val`
    }
  }

  def addAtHead(`val`: Int): Unit = {
    val newh = new SinglyLinkedList(`val`)
    newh.next = l
    l = newh
  }

  def addAtTail(`val`: Int): Unit = {
    if (l == null) {
      l = new SinglyLinkedList(`val`)
    } else {
      var h = l
      while (h.next != null) h = h.next
      h.next = new SinglyLinkedList(`val`)
    }
  }

  def addAtIndex(index: Int, `val`: Int): Unit = {
    if (index == 0) addAtHead(`val`)
    else if (l == null) ()
    else {
      var h = l
      for (_ <- 0 until index - 1) {
        if (h.next == null) return
        h = h.next
      }
      val next = h.next
      val newNode = new SinglyLinkedList(`val`)
      h.next = newNode
      newNode.next = next
    }
  }

  def deleteAtIndex(index: Int): Unit = {
    if (index == 0) {
      l = l.next
    } else {
      var h = l
      for (_ <- 0 until index - 1) {
        if (h.next == null) return
        h = h.next
      }
      val prev = h
      if (h.next == null) ()
      else {
        val next = h.next.next
        prev.next = next
      }
    }
  }
}
