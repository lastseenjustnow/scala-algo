package contests

import scala.collection.mutable

object BiweeklyContest107 {
  def maximumNumberOfStringPairs(words: Array[String]): Int = {
    val st: mutable.Set[String] = mutable.Set[String]()
    var c = 0
    for (w <- words) {
      if (st.contains(w(1).toString + w(0).toString)) c += 1
      else if (w(0) != w(1)) st += w
    }
    c
  }
}
