import scala.collection.mutable
import scala.math.abs

object MathProblems {
  def myPowRecursive(x: Double, n: Int): Double = {

    val memo: mutable.HashMap[Int, Double] = mutable.HashMap()

    def rec(thisN: Int): Double = {
      val isEven = thisN % 2
      thisN match {
        case thisN if thisN == 0 => 1
        case thisN if thisN == 1 => x
        case thisN if thisN == -1 => 1 / x
        case _ =>
          val left = memo.getOrElseUpdate(thisN / 2 + isEven, rec(thisN / 2 + isEven))
          val right = memo.getOrElseUpdate(thisN / 2, rec(thisN / 2))
          left * right
      }
    }

    rec(n)
  }

  def myPowIterative(x: Double, n: Int): Double = {
    var start = n match {
      case n if n == 0 => 1
      case n if n > 0 => x
      case n if n < 0 => 1 / x
    }
    var residual = 1D

    for (i <- abs(n).toBinaryString.drop(1).reverse) {
      residual = if (i == '1') start * residual else residual
      start *= start
    }
    start * residual
  }
}
