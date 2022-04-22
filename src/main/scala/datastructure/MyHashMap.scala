package datastructure

class MyHashMap {

  var arr: Array[Int] = Array()

  def put(key: Int, value: Int): Unit = {
    if (key > arr.length - 1) {
      arr = arr ++ Array.fill(key - arr.length + 1)(-1)
    }
    arr(key) = value
  }

  def get(key: Int): Int = {
    if (key > arr.length - 1) -1 else arr(key)
  }

  def remove(key: Int): Unit = {
    if (key > arr.length - 1) () else arr(key) = -1
  }
}
