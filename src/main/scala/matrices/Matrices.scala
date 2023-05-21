package matrices

import scala.annotation.tailrec

object Matrices {
  def gameOfLifeNaive(board: Array[Array[Int]]): Unit = {
    /** Naive solution.
     *
     * Space complexity: O(m x n)
     * */
    val (m, n) = (board(0).length, board.length)

    def countNeighbours(i: Int, j: Int): Int = {
      (((i - 1) max 0) to ((i + 1) min (m - 1)))
        .flatMap(absciss => (((j - 1) max 0) to ((j + 1) min (n - 1)))
          .map(ordinate => (absciss, ordinate)))
        .map { case (absciss: Int, ordinate: Int) => board(ordinate)(absciss) }.sum - board(j)(i)
    }

    def nextState(i: Int, j: Int): Int = {
      countNeighbours(i, j) match {
        case x if x < 2 || x > 3 => 0
        case x if x == 2 => board(j)(i)
        case x if x == 3 => 1
      }
    }

    val res = Array.fill(n)(Array.fill(m)(0))
    for (ordinate <- 0 until n; absciss <- 0 until m) res(ordinate)(absciss) = nextState(absciss, ordinate)
    for (ordinate <- 0 until n; absciss <- 0 until m) board(ordinate)(absciss) = res(ordinate)(absciss)
  }

  def gameOfLife(board: Array[Array[Int]]): Unit = {
    /**
     * An idea: while iterating, assigning new values, denoting both previous state and current state of the variable:
     *
     * prev_state -> current_state : denoting value
     * 1 -> 0 : -1
     * 0 -> 1 : 2
     * 1 -> 1 : 1
     * 0 -> 0 : 0
     *
     * Time complexity: O(m x n)
     * Space complexity: O(1)
     * */
    val (m, n) = (board(0).length, board.length)

    def countNeighbours(i: Int, j: Int): Int = {
      (((i - 1) max 0) to ((i + 1) min (m - 1)))
        .flatMap(absciss => (((j - 1) max 0) to ((j + 1) min (n - 1)))
          .map(ordinate => (absciss, ordinate)))
        .map {
          case (absciss: Int, ordinate: Int) if board(ordinate)(absciss) == -1 => 1
          case (absciss: Int, ordinate: Int) if board(ordinate)(absciss) == 2 => 0
          case (absciss: Int, ordinate: Int) => board(ordinate)(absciss)
        }.sum - board(j)(i)
    }

    def nextState(i: Int, j: Int): Int = {
      (board(j)(i), countNeighbours(i, j)) match {
        case (oldVal, neighbours) if oldVal == 1 && (neighbours < 2 || neighbours > 3) => -1
        case (_, neighbours) if neighbours < 2 || neighbours > 3 => 0
        case (oldVal, neighbours) if oldVal == 0 && neighbours == 3 => 2
        case (_, neighbours) if neighbours == 3 => 1
        case (oldVal, neighbours) if neighbours == 2 => oldVal
      }
    }

    for (ordinate <- 0 until n; absciss <- 0 until m) board(ordinate)(absciss) = nextState(absciss, ordinate)

    for (ordinate <- 0 until n; absciss <- 0 until m) {
      if (board(ordinate)(absciss) == 2) board(ordinate)(absciss) = 1
      else if (board(ordinate)(absciss) == -1) board(ordinate)(absciss) = 0
    }
  }


  def generateMatrix(n: Int): Array[Array[Int]] = {
    val it: Iterator[Int] = Iterator.from(1)
    val matrix = Array.fill(n)(Array.fill(n)(0))

    def iterateSquare(left: Int, right: Int): Unit = {
      right - left match {
        case x if x == 0 => matrix(left)(left) = it.next()
        case _ =>
          for (abscissa <- left until right) matrix(left)(abscissa) = it.next()
          for (ordinate <- left until right) matrix(ordinate)(right) = it.next()
          for (abscissa <- left until right) matrix(right)(right + left - abscissa) = it.next()
          for (ordinate <- left until right) matrix(right + left - ordinate)(left) = it.next()
      }
    }

    var (left, right) = (0, n - 1)
    while (left <= right) {
      iterateSquare(left, right)
      left += 1
      right -= 1
    }
    matrix
  }

  def countNegatives(grid: Array[Array[Int]]): Int = {
    val n = grid(0).length
    var negCount = n
    var res = 0
    for (row <- grid.reverse) {
      while (negCount != 0 && row(n - negCount) >= 0) negCount -= 1
      res += negCount
    }
    res
  }

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    val (n, m) = (matrix(0).length, matrix.length)
    var (left, right) = (0, n * m - 1)
    while (left <= right) {
      val mid = (right - left) / 2 + left
      val (midI, midJ) = (mid % n, mid / n)
      if (matrix(midJ)(midI) == target) return true
      else if (matrix(midJ)(midI) > target) right = mid - 1
      else left = mid + 1
    }
    false
  }

  def searchMatrix2DRec(matrix: Array[Array[Int]], target: Int): Boolean = {

    val m = matrix.head.length
    val n = matrix.length

    @tailrec
    def bs(left: Int, right: Int): Boolean = {
      if (left > right) false
      else {
        val middle = (right - left) / 2 + left
        val midElem = matrix(middle / m)(middle % m)
        if (midElem == target) true
        else if (midElem > target) bs(left, middle - 1)
        else bs(middle + 1, right)
      }
    }
    bs(0, m * n - 1)
  }

  def minimumAverageDifference(nums: Array[Int]): Int = {
    val n = nums.length
    if (n == 0) return 0
    nums
      .scanLeft((0, nums.sum))((x, y) => (x._1 + y, x._2 - y))
      .zipWithIndex
      .tail
      .map(x => (Math.abs(x._1._1 / x._2 - (if (n - x._2 == 0) 0 else x._1._2 / (n - x._2))), x._2 - 1))
      .min
      ._2
  }

  def countUnguarded(m: Int, n: Int, guards: Array[Array[Int]], walls: Array[Array[Int]]): Int = {
    val matrix: Array[Array[Int]] = Array.fill(m)(Array.fill(n)(0))
    var nonObserved = m * n
    for (wall <- walls) {
      matrix(wall(0))(wall(1)) = 2
      nonObserved -= 1
    }
    for (guard <- guards) {
      matrix(guard(0))(guard(1)) = 3
      nonObserved -= 1
    }

    for (guard <- guards) {
      var (i, j) = (guard(0) + 1, guard(1))

      // Down
      while (i < m && matrix(i)(j) < 2) {
        if (matrix(i)(j) == 0) {
          matrix(i)(j) = 1
          nonObserved -= 1
        }
        i += 1
      }

      // Up
      i = guard(0) - 1
      j = guard(1)
      while (i >= 0 && matrix(i)(j) < 2) {
        if (matrix(i)(j) == 0) {
          matrix(i)(j) = 1
          nonObserved -= 1
        }
        i -= 1
      }

      // Right
      i = guard(0)
      j = guard(1) + 1
      while (j < n && matrix(i)(j) < 2) {
        if (matrix(i)(j) == 0) {
          matrix(i)(j) = 1
          nonObserved -= 1
        }
        j += 1
      }

      // Left
      i = guard(0)
      j = guard(1) - 1
      while (j >= 0 && matrix(i)(j) < 2) {
        if (matrix(i)(j) == 0) {
          matrix(i)(j) = 1
          nonObserved -= 1
        }
        j -= 1
      }

    }
    nonObserved
  }

}
