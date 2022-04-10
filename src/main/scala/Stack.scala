import scala.collection.mutable

object Stack {
  def calPoints(ops: Array[String]): Int = {
    /** You are keeping score for a baseball game with strange rules.
     * The game consists of several rounds, where the scores of past rounds may affect future rounds' scores.
     *
     * At the beginning of the game, you start with an empty record.
     * You are given a list of strings ops, where ops[i] is the ith operation you must apply to the record and is one of the following:
     *
     * An integer x - Record a new score of x.
     * "+" - Record a new score that is the sum of the previous two scores. It is guaranteed there will always be two previous scores.
     * "D" - Record a new score that is double the previous score. It is guaranteed there will always be a previous score.
     * "C" - Invalidate the previous score, removing it from the record. It is guaranteed there will always be a previous score.
     * Return the sum of all the scores on the record. */

    val stack = mutable.Stack[Int]()

    for (symbol <- ops) {
      symbol match {
        case letter if letter == "D" => stack.push(stack.top * 2)
        case letter if letter == "C" => stack.pop()
        case letter if letter == "+" =>
          val first = stack.pop()
          val second = stack.pop()
          for (elem <- Array(second, first, first + second)) stack.push(elem)
        case _ => stack.push(symbol.toInt)
      }
    }
    stack.sum
  }
}
