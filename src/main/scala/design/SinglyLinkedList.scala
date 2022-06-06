package design

class SinglyLinkedList {
  var `val` = 0
  var next: SinglyLinkedList = null

  def this(x: Int) {
    this()
    `val` = x
  }
}
