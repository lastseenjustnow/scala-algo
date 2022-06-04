import design.Robot

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

  def solveNQueens(n: Int): List[List[String]] = {
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

    var res: List[List[String]] = List()

    def rec(row: Int, acc: List[String]): Unit = {
      for (col <- 0 until n) {
        if (!isUnderAttack(row, col)) {
          val newAcc = acc :+ Array.fill(n)('.').updated(col, 'Q').mkString
          placeQueen(row, col)
          if (row + 1 == n) res = res :+ newAcc else rec(row + 1, newAcc)
          removeQueen(row, col)
        }
      }
    }

    rec(0, List())
    res
  }

  def cleanRoom(robot: Robot): Unit = {
    var cleanedCells: Set[(Int, Int)] = Set()
    var di = (0, 1)

    def turnRightDirection(t: Int): Unit = {
      val a = Array((0, 1), (1, 0), (0, -1), (-1, 0))
      val i = a.indexWhere(x => x == di)
      (0 until t).foreach(_ => robot.turnRight())
      di = a((i + t) % 4)
    }

    def turnLeftDirection(t: Int): Unit = {
      val a = Array((0, 1), (1, 0), (0, -1), (-1, 0))
      val i = a.indexWhere(x => x == di)
      (0 until t).foreach(_ => robot.turnLeft())
      di = a((4 + (i - t)) % 4)
    }

    def place(cell: (Int, Int)): Unit = {
      robot.clean()
      cleanedCells = cleanedCells + cell
    }

    def isValid(cell: (Int, Int)): Boolean = !cleanedCells.contains(cell)

    def remove(): Unit = {
      robot.turnRight()
      robot.turnRight()
      robot.move()
      robot.turnRight()
      robot.turnRight()
    }

    def backtrack(cell: (Int, Int)): Unit = {
      for (t <- 0 to 3) {
        turnRightDirection(t)
        if (isValid((cell._1 + di._1, cell._2 + di._2)) && robot.move()) {
          place(cell._1 + di._1, cell._2 + di._2)
          backtrack(cell._1 + di._1, cell._2 + di._2)
          remove()
          backtrack(cell)
        }
        turnLeftDirection(t)
      }
    }

    robot.clean()
    backtrack((0, 0))
  }
}
