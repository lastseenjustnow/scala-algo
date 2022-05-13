object Recursion {
  def totalNQueens(n: Int): Int = {
    var cols: Set[Int] = Set()
    var leftDiags: Set[Int] = Set()
    var rightDiags: Set[Int] = Set()

    def isUnderAttack(x: Int, y: Int): Boolean = cols.contains(y) || leftDiags.contains(y + x) || rightDiags.contains(y - x)

    def placeQueen(x: Int, y: Int): Unit = {
      cols = cols + y
      leftDiags = leftDiags + (y + x)
      rightDiags = rightDiags + (y - x)
    }

    def removeQueen(x: Int, y: Int): Unit = {
      cols = cols - y
      leftDiags = leftDiags - (y + x)
      rightDiags = rightDiags - (y - x)
    }

    def rec(row: Int, c: Int): Int = {
      var count = c
      for (col <- 0 until n) {
        if (!isUnderAttack(row, col)) {
          placeQueen(row, col)
          if (row + 1 == n) count += 1 else count = rec(row + 1, count)
          removeQueen(row, col)
        }
      }
      count
    }

    rec(0, 0)
  }
}
