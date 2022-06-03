package matrices

class NumMatrix(_matrix: Array[Array[Int]]) {

  private val m = _matrix.length // rows
  private val n = _matrix(0).length // columns

  private def prevSum(i: Int, j: Int): Int = if (i < 0 || j < 0) 0 else _matrix(i)(j)

  for (i <- 0 until m; j <- 0 until n) {
    _matrix(i)(j) = prevSum(i - 1, j) + prevSum(i, j - 1) - prevSum(i - 1, j - 1) + _matrix(i)(j)
  }

  def sumRegion(row1: Int, col1: Int, row2: Int, col2: Int): Int = {
    val val1 = prevSum(row2, col2)
    val val2 = prevSum(row2, col1 - 1)
    val val3 = prevSum(row1 - 1, col2)
    val val4 = prevSum(row1 - 1, col1 - 1)
    val1 - val2 - val3 + val4
  }

}
