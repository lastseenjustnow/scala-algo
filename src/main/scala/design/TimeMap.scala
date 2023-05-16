package design

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}

class TimeMap() {

  val map: mutable.Map[String, ArrayBuffer[(Int, String)]] = mutable.Map[String, ArrayBuffer[(Int, String)]]().withDefaultValue(ArrayBuffer.empty)

  def set(key: String, value: String, timestamp: Int): Unit = {
    map.getOrElseUpdate(key, ArrayBuffer.empty) += ((timestamp, value))
  }

  def get(key: String, timestamp: Int): String = {
    val thisVal = map(key)
    var left = 0
    var right = thisVal.length - 1
    while (left <= right) {
      val middle = (right - left) / 2 + left
      if (thisVal(middle)._1 <= timestamp && (middle == thisVal.length - 1 || thisVal(middle + 1)._1 > timestamp)) return thisVal(middle)._2
      else if (thisVal(middle)._1 > timestamp) right = middle - 1
      else left = middle + 1
    }
    ""
  }

}