package datastructure

import scala.annotation.tailrec


class ListNode(_x: Int = 0, _next: ListNode = null) {

  /**
   * Definition for singly-linked list.
   * */
  var next: ListNode = _next
  var x: Int = _x


  def toArray: Array[Int] = {
    @tailrec
    def rec(thisElem: ListNode, arr: Array[Int]): Array[Int] = {
      thisElem match {
        case null => arr
        case _ => rec(thisElem.next, arr :+ thisElem.x)
      }
    }

    rec(this, Array())
  }

}

