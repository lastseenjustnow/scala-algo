package datastructure

class MyStack() {

  var q: List[Int] = List()

  def push(x: Int): Unit = {
    q = x +: q
  }

  def pop(): Int = {
    val head = q.head
    q = q.tail
    head
  }

  def top(): Int = {
    q.head
  }

  def empty(): Boolean = {
    q.isEmpty
  }

}
