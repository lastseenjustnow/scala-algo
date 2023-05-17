package design

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}

class SnapshotArray(_length: Int) {

  private val map: mutable.Map[Int, ArrayBuffer[(Int, Int)]] = mutable.Map[Int, ArrayBuffer[(Int,Int)]]()
  private var snapshotNumber: Int = 0

  def set(index: Int, `val`: Int): Unit = {
    map.getOrElseUpdate(index, ArrayBuffer.empty) += ((snapshotNumber, `val`))
  }

  def snap(): Int = {
    snapshotNumber += 1
    snapshotNumber - 1
  }

  def get(index: Int, snap_id: Int): Int = {
    val thisVal = map.getOrElse(index, ArrayBuffer.empty)
    var left = 0
    var right = thisVal.length - 1
    while (left <= right) {
      val middle = (right - left) / 2 + left
      if (thisVal(middle)._1 <= snap_id && (middle == thisVal.length - 1 || thisVal(middle + 1)._1 > snap_id)) return thisVal(middle)._2
      else if (thisVal(middle)._1 > snap_id) right = middle - 1
      else left = middle + 1
    }
    0
  }

}