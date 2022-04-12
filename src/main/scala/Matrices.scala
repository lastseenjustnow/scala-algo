object Matrices {
  def gameOfLife(board: Array[Array[Int]]): Unit = {
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
}
