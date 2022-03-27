import SortingAndSearching._
import org.scalatest.FunSuite

class SortingAndSearchingTest extends FunSuite {
  test("Pow(x, n)") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(3, 2, 1, 5, 6, 4), 2, 5),
        (Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4, 4),

      )

    val Eps = 1e-7

    for (cond <- conditions) {
      assert(findKthLargest(cond._1, cond._2) == cond._3)
    }
  }


  test("Binary Search") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(-1, 0, 3, 5, 9, 12), 9, 4),
        (Array(-1, 0, 3, 5, 9, 12), 2, -1),
        (Array(-1, 0, 3, 5, 9, 12), 3, 2),
        (Array(-1, 0, 3, 5, 9, 12), -1, 0),
        (Array(-1, 0, 3, 5, 6, 9, 12), 6, 4),
        (Array(-1, 0, 3, 5, 6, 9, 12), 12, 6),
        (Array(-1, 0, 3, 5, 6, 9, 12), 9, 5),
        (Array(-1, 0, 3, 5, 6, 9, 12), 0, 1),
        (Array(-4, -1, 0, 3, 5, 6, 9, 12), 0, 2)
      )

    for (cond <- conditions) {
      assert(search(cond._1, cond._2) == cond._3)
    }
  }

  test("Search Insert Position") {
    val conditions: Array[(Array[Int], Int, Int)] =
      Array(
        (Array(1, 3, 5, 6), 5, 2),
        (Array(1, 3, 5, 6), 7, 4),
        (Array(1, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 0, 0),
        (Array(1, 2, 3, 5, 6), 2, 1),
        (Array(1, 2, 3, 5, 6), 6, 4),
        (Array(1, 2, 3, 5, 6), 3, 2),
        (Array(1, 2, 2, 2, 3, 5, 6), 2, 3),
        (Array(1, 2, 2, 2, 3, 5, 6), 5, 5)

      )

    for (cond <- conditions) {
      assert(searchInsert(cond._1, cond._2) == cond._3)
    }
  }

  test("Binary Search Triplets") {
    val conditions: Array[(Array[(Int, Int, Int)], Int, Int)] =
      Array(
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 5, 3),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 6, 4),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 8, 5),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 9, 23), (8, 10, 29), (9, 12, 43)), 8, 6),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 9, 23), (8, 10, 29), (9, 12, 43)), 8, 6),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 9, 23), (8, 10, 29), (9, 12, 43)), 8, 6),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 8, 18), (7, 8, 18), (8, 8, 18), (9, 8, 18), (10, 8, 18), (11, 9, 23), (12, 10, 29), (13, 12, 43)), 8, 10),
        (Array((0, 0, 0), (1, 1, 0), (2, 3, 2), (3, 4, 4), (4, 6, 10), (5, 8, 18), (6, 9, 23), (7, 10, 29), (8, 12, 43)), 12, 8)
      )

    for (cond <- conditions) {
      assert(tripletBinarySearch(cond._1, cond._2) == cond._3)
    }
  }

}
