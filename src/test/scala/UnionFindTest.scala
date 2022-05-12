import org.scalatest.FunSuite
import DisjointSet._
import datastructure.graph.QuickUnion


class UnionFindTest extends FunSuite {
  test("Test Union Find data structure") {
    val uf = new QuickUnion(10)
    uf.union(1, 2)
    uf.union(2, 5)
    uf.union(5, 6)
    uf.union(6, 7)
    uf.union(3, 8)
    uf.union(8, 9)
    assert(uf.connected(1, 5))
    assert(uf.connected(5, 7))
    assert(!uf.connected(4, 9))
    uf.union(9, 4)
    assert(uf.connected(4, 9))
  }

  test("Number of Provinces") {
    val conditions: Array[(Array[Array[Int]], Int)] =
      Array(
        (Array(Array(1, 1, 0), Array(1, 1, 0), Array(0, 0, 1)), 2),
        (Array(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1)), 3),
        (Array(Array(1, 1, 1), Array(1, 1, 1), Array(1, 1, 1)), 1),
      )

    for (cond <- conditions) {
      assert(findCircleNum(cond._1) == cond._2)
    }
  }

  test("Graph Valid Tree") {
    val conditions: Array[(Array[Array[Int]], Int, Boolean)] =
      Array(
        (Array(Array(0, 1), Array(0, 2), Array(0, 3), Array(1, 4)), 5, true),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(1, 3), Array(1, 4)), 5, false),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(1, 3), Array(1, 4), Array(6, 7)), 7, false),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(0, 4), Array(0, 5), Array(1, 6), Array(6, 7)), 8, true),
        (Array(Array(0, 1), Array(2, 3), Array(1, 2)), 4, true)
      )

    for (cond <- conditions) {
      assert(validTree(cond._2, cond._1) == cond._3)
    }
  }

  test("Number of Connected Components in an Undirected Graph") {
    val conditions: Array[(Array[Array[Int]], Int, Int)] =
      Array(
        (Array(Array(0, 1), Array(1, 2), Array(3, 4)), 5, 2),
        (Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(3, 4)), 5, 1)
      )

    for (cond <- conditions) {
      assert(countComponents(cond._2, cond._1) == cond._3)
    }
  }

  test("The Earliest Moment When Everyone Become Friends") {
    val conditions: Array[(Array[Array[Int]], Int, Int)] =
      Array(
        (Array(Array(20190101, 0, 1), Array(20190104, 3, 4), Array(20190107, 2, 3), Array(20190211, 1, 5), Array(20190224, 2, 4), Array(20190301, 0, 3), Array(20190312, 1, 2), Array(20190322, 4, 5)), 6, 20190301),
        (Array(Array(0, 2, 0), Array(1, 0, 1), Array(3, 0, 3), Array(4, 1, 2), Array(7, 3, 1)), 4, 3),
        (Array(Array(9, 3, 0), Array(0, 2, 1), Array(8, 0, 1), Array(1, 3, 2), Array(2, 2, 0), Array(3, 3, 1)), 4, 2)
      )

    for (cond <- conditions) {
      assert(earliestAcq(cond._1, cond._2) == cond._3)
    }
  }

  test("Smallest String With Swaps") {
    val conditions: Array[(String, List[List[Int]], String)] =
      Array(
        ("dcab", List(List(0, 3), List(1, 2)), "bacd"),
        ("dcab", List(List(0, 3), List(1, 2), List(0, 2)), "abcd"),
        ("cba", List(List(0, 1), List(1, 2)), "abc")
      )

    for (cond <- conditions) {
      assert(smallestStringWithSwaps(cond._1, cond._2) == cond._3)
    }
  }

  test("Evaluate Division") {
    val conditions: Array[(List[List[String]], Array[Double], List[List[String]], Array[Double])] =
      Array(
        (List(List("a", "b"), List("b", "c")), Array(2.0, 3.0), List(List("a", "c"), List("b", "a"), List("a", "e"), List("a", "a"), List("x", "x")), Array(6.00000, 0.50000, -1.00000, 1.00000, -1.00000)),
        (List(List("a", "b"), List("b", "c"), List("bc", "cd")), Array(1.5, 2.5, 5.0), List(List("a", "c"), List("c", "b"), List("bc", "cd"), List("cd", "bc")), Array(3.75000, 0.40000, 5.00000, 0.20000)),
        (List(List("a", "b")), Array(0.5), List(List("a", "b"), List("b", "a"), List("a", "c"), List("x", "y")), Array(0.5, 2.00000, -1.00000, -1.00000)),
        (List(List("x1", "x2"), List("x2", "x3"), List("x1", "x4"), List("x2", "x5")), Array(3.0, 0.5, 3.4, 5.6), List(List("x2", "x4"), List("x1", "x5"), List("x1", "x3"), List("x5", "x5"), List("x5", "x1"), List("x3", "x4"), List("x4", "x3"), List("x6", "x6"), List("x0", "x0")), Array(1.1333333333333333, 16.8, 1.5, 1.0, 0.05952380952380953, 2.2666666666666666, 0.44117647058823534, -1.0, -1.0)),
        (List(List("a", "e"), List("b", "e")), Array(4.0, 3.0), List(List("a", "b"), List("e", "e"), List("x", "x")), Array(1.3333333333333333, 1.00000, -1.00000)),
        (List(List("a", "b"), List("e", "f"), List("b", "e")), Array(3.4, 1.4, 2.3), List(List("b", "a"), List("a", "f"), List("f", "f"), List("e", "e"), List("c", "c"), List("a", "c"), List("f", "e")), Array(0.29411764705882354, 10.947999999999999, 1.0, 1.0, -1.0, -1.0, 0.7142857142857143)),
        (List(List("a", "b"), List("c", "d")), Array(1.0, 1.0), List(List("a", "c"), List("b", "d"), List("b", "a"), List("d", "c")), Array(-1.00000, -1.00000, 1.00000, 1.00000)),
        (List(List("a", "b"), List("c", "b"), List("d", "b"), List("w", "x"), List("y", "x"), List("z", "x"), List("w", "d")), Array(2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), List(List("a", "c"), List("b", "c"), List("a", "e"), List("a", "a"), List("x", "x"), List("a", "z")), Array(0.6666666666666666, 0.3333333333333333, -1.00000, 1.00000, 1.00000, 0.04464285714285714))
      )

    for (cond <- conditions) {
      assert(calcEquation(cond._1, cond._2, cond._3).toList == cond._4.toList)
    }
  }

  test("Optimize Water Distribution in a Village") {
    val conditions: Array[(Int, Array[Int], Array[Array[Int]], Int)] =
      Array(
        (3, Array(1, 2, 2), Array(Array(1, 2, 1), Array(2, 3, 1)), 3),
        (2, Array(1, 1), Array(Array(1, 2, 1), Array(1, 2, 2)), 2),
        (3, Array(5, 3, 1), Array(Array(2, 1, 2), Array(3, 1, 4)), 6),
        (5, Array(46012, 72474, 64965, 751, 33304), Array(Array(2, 1, 6719), Array(3, 2, 75312), Array(5, 3, 44918)), 131704),
        (6, Array(4625, 65696, 86292, 68291, 37147, 7880), Array(Array(2, 1, 79394), Array(3, 1, 45649), Array(4, 1, 75810), Array(5, 3, 22340), Array(6, 1, 6222)), 204321),
        (3, Array(62693, 87782, 24745), Array(Array(2, 1, 11653), Array(3, 2, 13613)), 50011),
        (50, Array(62693, 87782, 78682, 81671, 24745, 65255, 78647, 44719, 8331, 90816, 72429, 27535, 38548, 26329, 56884, 61337, 54924, 89648, 60045, 68882, 8146, 86370, 88355, 17526, 26113, 49779, 43781, 88498, 92375, 4917, 14854, 54266, 55725, 75875, 40380, 56552, 19520, 40417, 10838, 38815, 17422, 22943, 96316, 109, 92321, 12420, 21717, 4707, 68733, 70199), Array(Array(2, 1, 11653), Array(5, 2, 13613)), 2374682),
        (50, Array(62693, 87782, 78682, 81671, 24745, 65255, 78647, 44719, 8331, 90816, 72429, 27535, 38548, 26329, 56884, 61337, 54924, 89648, 60045, 68882, 8146, 86370, 88355, 17526, 26113, 49779, 43781, 88498, 92375, 4917, 14854, 54266, 55725, 75875, 40380, 56552, 19520, 40417, 10838, 38815, 17422, 22943, 96316, 109, 92321, 12420, 21717, 4707, 68733, 70199), Array(Array(2, 1, 11653), Array(5, 2, 13613), Array(6, 3, 63921), Array(7, 4, 20988), Array(8, 5, 34053), Array(9, 2, 57779), Array(11, 5, 39145), Array(12, 10, 96742), Array(13, 7, 2696), Array(15, 4, 93103), Array(16, 11, 76857), Array(17, 3, 38026), Array(18, 7, 50670), Array(19, 14, 51565), Array(20, 5, 21487), Array(22, 13, 79172), Array(23, 3, 58291), Array(24, 17, 23634), Array(25, 8, 66553), Array(26, 19, 84892), Array(27, 20, 17595), Array(28, 4, 88853), Array(29, 5, 78073), Array(31, 22, 59100), Array(32, 29, 32588), Array(33, 25, 43304), Array(34, 31, 16139), Array(35, 3, 71565), Array(36, 26, 6076), Array(37, 3, 70712), Array(38, 26, 73943), Array(39, 8, 551), Array(40, 30, 84528), Array(41, 34, 31055), Array(42, 18, 43503), Array(43, 19, 56259), Array(44, 8, 36845), Array(45, 44, 69872), Array(46, 18, 86220), Array(47, 35, 55960), Array(48, 37, 51081), Array(50, 28, 69716)), 1638068),
        (4, Array(54743, 78618, 69161, 49332), Array(Array(2, 1, 68029), Array(3, 2, 14198), Array(4, 3, 57918)), 176191),
        (60, Array(93151, 20876, 59743, 57253, 22852, 68389, 7424, 54743, 32955, 39509, 14896, 54179, 51356, 78618, 95595, 69161, 37790, 67284, 91644, 91111, 52096, 61039, 56597, 70549, 72491, 90473, 42299, 76091, 89905, 31271, 58546, 48511, 72171, 78695, 41038, 81168, 32922, 49332, 637, 7340, 70333, 20202, 45698, 64674, 12549, 46263, 26798, 1334, 30355, 83189, 26439, 51031, 85145, 56095, 38430, 79718, 82385, 25719, 97525, 82106), Array(Array(2, 1, 21154), Array(3, 2, 81115), Array(4, 3, 94841), Array(5, 2, 96414), Array(6, 3, 72515), Array(7, 5, 52265), Array(8, 1, 60281), Array(9, 5, 47008), Array(10, 6, 83062), Array(11, 1, 83592), Array(12, 11, 29667), Array(13, 5, 43482), Array(14, 8, 68029), Array(15, 6, 29058), Array(16, 14, 14198), Array(17, 8, 61513), Array(18, 10, 96383), Array(19, 3, 12103), Array(21, 11, 51835), Array(22, 8, 14803), Array(23, 22, 30324), Array(24, 23, 63187), Array(25, 21, 62508), Array(26, 13, 86421), Array(27, 22, 59810), Array(28, 6, 80818), Array(29, 25, 350), Array(30, 26, 9676), Array(31, 27, 11396), Array(33, 29, 39112), Array(34, 18, 35099), Array(35, 3, 79588), Array(36, 25, 93238), Array(37, 30, 18366), Array(38, 16, 57918), Array(39, 36, 14416), Array(40, 25, 27362), Array(41, 5, 12434), Array(42, 9, 5570), Array(43, 42, 72309), Array(44, 8, 81276), Array(45, 44, 2620), Array(46, 44, 57766), Array(47, 11, 71293), Array(48, 40, 14627), Array(49, 48, 33901), Array(52, 49, 70471), Array(53, 38, 6615), Array(55, 19, 77453), Array(56, 9, 63999), Array(57, 34, 10940), Array(58, 29, 43449), Array(59, 43, 22295), Array(60, 5, 84242)), 2032576),
        (5, Array(7, 9, 14, 2, 3), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 13),
        (5, Array(7, 9, 1, 2, 3), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 12),
        (6, Array(7, 9, 1, 2, 3, 8), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 20),
        (6, Array(7, 1, 1, 2, 3, 8), Array(Array(1, 2, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 19),
        (7, Array(7, 1, 1, 2, 3, 8, 3), Array(Array(7, 4, 4), Array(2, 3, 2), Array(3, 5, 6), Array(2, 4, 2)), 25),
      )

    for (cond <- conditions) {
      assert(minCostToSupplyWater(cond._1, cond._2, cond._3) == cond._4)
    }
  }

}
