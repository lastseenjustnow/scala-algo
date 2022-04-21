package datastructure

class MyHashSet() {

  var arr: Array[Option[Int]] = Array()

  def add(key: Int): Unit = {
    if (key > arr.length - 1) {
      arr = arr ++ Array.fill(key - arr.length + 1)(None)
    }
    arr(key) = Some(key)
  }

  def remove(key: Int): Unit = {
    if (key < arr.length) arr(key) = None
  }

  def contains(key: Int): Boolean = {
    if (key > arr.length - 1) return false
    arr(key) match {
      case Some(_) => true
      case None => false
    }
  }
}
