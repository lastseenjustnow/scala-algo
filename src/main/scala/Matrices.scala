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

}