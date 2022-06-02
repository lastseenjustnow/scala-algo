import SortingAndSearching.{maxDistanceFunctional, mySqrt, tripletBinarySearch}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.math.{abs, max}

object ArraysAndStrings {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val hash: mutable.HashMap[Int, Int] = mutable.HashMap()
    val solution: Array[Int] = Array.fill(2)(0)
    var flag: Boolean = false
    var i: Int = 0

    while (i < nums.length && !flag) {
      if (hash.contains(target - nums(i))) {
        flag = true
        solution(0) = hash(target - nums(i))
        solution(1) = i
      }
      hash(nums(i)) = i
      i += 1
    }
    solution
  }

  def twoSumReturn(nums: Array[Int], target: Int): Array[Int] = {
    val st: mutable.Map[Int, Int] = mutable.Map()
    for (i <- nums.indices) {
      if (st.contains(target - nums(i))) {
        return Array(st(target - nums(i)), i)
      }
      st += (nums(i) -> i)
    }
    Array()
  }

  def twoSumBuffer(nums: Array[Int], target: Int): Array[(Int, Int)] = {
    val st: mutable.Set[Int] = mutable.Set()
    val res: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()

    for (i <- nums.indices) {
      val thisNum: Int = nums(i)
      val thatNum: Int = target - thisNum
      if (st.contains(thatNum)) {
        val tup = (thatNum, thisNum)
        res += tup
      }
      st += thisNum
    }
    res.toArray
  }

  def maxOperations(nums: Array[Int], k: Int): Int = {
    val map: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
    var res = 0
    for (elem <- nums) {
      if (map.getOrElse(k - elem, 0) != 0) {
        res += 1
        map.update(k - elem, map(k - elem) - 1)
      } else map.update(elem, map.getOrElse(elem, 0) + 1)
    }
    res
  }

  def myAtoi(s: String): Int = {
    val regexp = raw"^\s*([-|+]{0,1}\d+).*".r
    s match {
      case regexp(x) => ((BigInt(x) min Int.MaxValue) max Int.MinValue).toInt
      case _ => 0
    }
  }

  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    /**
     * You are given an array of non-overlapping intervals where intervals[i] = [starti, endi] represent the start and the end of the ith interval
     * and intervals is sorted in ascending order by starti.
     * You are also given an interval newInterval = [start, end] that represents the start and end of another interval.
     *
     * Insert newInterval into intervals such that intervals is still sorted in ascending order by starti
     * and intervals still does not have any overlapping intervals (merge overlapping intervals if necessary).
     *
     * Return intervals after the insertion.
     * */

    val updatedInterval = newInterval
    val res: ArrayBuffer[Array[Int]] = ArrayBuffer()
    var i = 0

    while (i < intervals.length && updatedInterval(0) > intervals(i)(1)) {
      res += intervals(i)
      i += 1
    }

    while (i < intervals.length && updatedInterval(1) >= intervals(i)(0)) {
      updatedInterval(0) = updatedInterval(0) min intervals(i)(0)
      updatedInterval(1) = updatedInterval(1) max intervals(i)(1)
      i += 1
    }

    res += updatedInterval

    while (i < intervals.length) {
      res += intervals(i)
      i += 1
    }

    res.toArray
  }

  def findPairs(nums: Array[Int], k: Int): Int = {
    /**
     * Given an array of integers nums and an integer k, return the number of unique k-diff pairs in the array.
     *
     * A k-diff pair is an integer pair (nums[i], nums[j]), where the following are true:
     *
     * 0 <= i, j < nums.length
     * i != j
     * nums[i] - nums[j] == k
     * Notice that |val| denotes the absolute value of val.
     * */
    val res: mutable.Set[(Int, Int)] = mutable.Set()
    val diffs: mutable.Set[Int] = mutable.Set()

    for (i <- nums.indices) {
      if (diffs.contains(nums(i) - k)) {
        val minVal = nums(i) min (nums(i) - k)
        val maxVal = nums(i) max (nums(i) - k)
        val pair = (minVal, maxVal)
        res += pair
      }
      if (diffs.contains(nums(i) + k)) {
        val minVal = nums(i) min (nums(i) + k)
        val maxVal = nums(i) max (nums(i) + k)
        val pair = (minVal, maxVal)
        res += pair
      }
      diffs += nums(i)
    }
    res.toSeq.length
  }

  def minWastedSpaceBruteForce(packages: Array[Int], boxes: Array[Array[Int]]): Int = {
    var res = Int.MaxValue
    var i = 0
    val packagesSorted = packages.sorted
    val n = packages.length

    while (i < boxes.length) {
      if (!(packages.max > boxes(i).max)) {
        val boxesSorted = boxes(i).sorted
        var (j, m, thisResidual) = (0, 0, 0)
        while (j < n) {
          if (packagesSorted(j) <= boxesSorted(m)) {
            thisResidual += boxesSorted(m) - packagesSorted(j)
            j += 1
          } else {
            m += 1
          }
        }
        res = res min thisResidual
      }
      i += 1
    }
    if (res == Int.MaxValue) -1 else (res % (Math.pow(10, 9) + 7)).toInt
  }

  def minWastedSpace(packages: Array[Int], boxes: Array[Array[Int]]): Int = {

    val packagesMax = packages.max
    val firstBoxWaste: Array[(Int, Long, Long)] = packages
      .sorted
      .scanLeft((0L, 0L))((t, pack) => (t._1 + pack, pack))
      .zipWithIndex
      .map {
        case ((cumsum: Long, pack: Long), index: Int) => (index, pack, pack * index - cumsum)
      }

    var res = Long.MaxValue

    for (box <- boxes) {
      if (!(packagesMax > box.max)) {
        val boxesSorted = box.sorted
        var (j, prevBoxIndex, thisResidual: Long) = (0, 0, 0L)
        while (j < box.length) {
          val prevBox = firstBoxWaste(prevBoxIndex)

          val currentBoxIndex = tripletBinarySearch(firstBoxWaste, boxesSorted(j), prevBoxIndex)
          val currentBox = firstBoxWaste(currentBoxIndex)

          val thisWastedSpace: Long = currentBox._3 - prevBox._3 - (currentBox._2 - prevBox._2) * prevBox._1.toLong + (boxesSorted(j).toLong - currentBox._2) * (currentBox._1.toLong - prevBox._1.toLong)
          thisResidual += thisWastedSpace
          prevBoxIndex = currentBoxIndex
          j += 1
        }
        res = res min thisResidual
      }
    }
    if (res == Long.MaxValue) -1 else (res % (Math.pow(10, 9).toLong + 7)).toInt
  }

  def lengthOfLongestSubstring(s: String): Int = {
    val n = s.length
    var (localMax, globalMax) = (0, 0)
    var i = 0
    val st: mutable.HashMap[Char, Int] = mutable.HashMap()

    while (i < n) {
      if (i - localMax > st.getOrElse(s(i), Int.MinValue)) {
        localMax += 1
        globalMax = globalMax max localMax
      } else {
        localMax = i - st(s(i))
      }
      st(s(i)) = i
      i += 1
    }
    globalMax
  }

  def maxAreaBruteForce(height: Array[Int]): Int = {
    /**
     * Trivial.
     *
     * Time complexity: O(n ** 2)
     * */
    var maxVal = 0

    for (i <- height.indices) {
      for (j <- i until height.length) {
        maxVal = maxVal max ((height(i) min height(j)) * abs(i - j))
      }
    }
    maxVal
  }

  def maxArea(height: Array[Int]): Int = {
    /** *
     * Time complexity: O(n)
     * */

    def squareF(left: Int, right: Int): Int = {
      (height(left) min height(right)) * (right - left)
    }

    var start = 0
    var end = height.length - 1
    var res = squareF(start, end)

    while (start < end) {
      if (height(start) <= height(end)) {
        start += 1
      } else {
        end -= 1
      }
      res = res max squareF(start, end)
    }
    res
  }

  def maxAreaRecursive(height: Array[Int]): Int = {
    /** *
     * Time complexity: O(n)
     * */

    def squareF(left: Int, right: Int): Int = {
      (height(left) min height(right)) * (right - left)
    }

    @tailrec
    def rec(left: Int, right: Int, maxarea: Int): Int = {
      (left, right) match {
        case (l, r) if r == l => maxarea
        case (l, r) if height(l) >= height(r) => rec(l, r - 1, maxarea max squareF(l, r))
        case _ => rec(left + 1, right, maxarea max squareF(left, right))
      }
    }

    rec(0, height.length - 1, squareF(0, height.length - 1))
  }

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    val res = nums.count(x => x != `val`)
    for ((elem, i) <- nums.filter(x => x != `val`).zipWithIndex) {
      nums(i) = elem
    }
    res
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    var (start, end) = (0, 0)
    while (end < nums.length) {
      if (nums(start) != nums(end)) {
        start += 1
      }
      nums(start) = nums(end)
      end += 1
    }
    start + 1
  }

  def checkIfExist(arr: Array[Int]): Boolean = {
    val st = mutable.Set[Int]()
    for (elem <- arr) {
      if (st.contains(elem * 2) || (st.contains(elem / 2) && elem % 2 != 1)) return true
      else st += elem
    }
    false
  }

  def validMountainArray(arr: Array[Int]): Boolean = {
    val n = arr.length
    var i = 1
    if (n < 3 || arr(i) < arr(i - 1)) {
      return false
    }

    while (i < n - 1 && arr(i) > arr(i - 1)) {
      i += 1
    }

    while (i < n && arr(i) < arr(i - 1)) {
      i += 1
    }
    if (i == n) true else false
  }

  def intToRoman(num: Int): String = {
    val arabToRom = Map(1 -> "I", 5 -> "V", 10 -> "X", 50 -> "L", 100 -> "C", 500 -> "D", 1000 -> "M")
    var result: String = arabToRom(1000) * (num / 1000)
    val numString = num.toString
    val n = numString.length
    var power = (num - (num / 1000) * 1000).toString.length - 1

    while (power >= 0) {
      val digitInt = numString(n - power - 1).asDigit
      val multiplier = Math.pow(10, power).toInt
      if (digitInt == 4 || digitInt == 9) {
        result = result.concat(arabToRom(multiplier))
        result = result.concat(arabToRom((digitInt + 1) * multiplier))
      } else {
        result = result.concat(arabToRom(5 * multiplier) * (digitInt / 5))
        result = result.concat(arabToRom(multiplier) * (digitInt % 5))
      }
      power -= 1
    }
    result
  }

  def romanToInt(s: String): Int = {
    val map: Map[Char, Int] = Map(
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000)

    var (i, res) = (0, 0)

    while (i < s.length) {
      if (i == s.length - 1 || map(s(i + 1)) <= map(s(i))) {
        res += map(s(i))
      } else {
        res += map(s(i + 1)) - map(s(i))
        i += 1
      }
      i += 1
    }
    res
  }

  def isValid(s: String): Boolean = {
    var stack: ListBuffer[Char] = ListBuffer()
    var (stackLength, leftLength) = (0, s.length)
    val map: Map[Char, Char] = Map(')' -> '(', ']' -> '[', '}' -> '{')
    var i = 0
    var flag = true

    while (flag && i < s.length) {
      if (!map.contains(s(i))) {
        stack += s(i)
        stackLength += 1
      } else if (stack.isEmpty || stack.last != map(s(i))) {
        flag = false
      } else {
        stack = stack.dropRight(1)
        stackLength -= 1
      }
      leftLength -= 1
      if (stackLength > leftLength) {
        flag = false
      }
      i += 1
    }
    if (stack.nonEmpty) false else flag
  }

  def replaceElements(arr: Array[Int]): Array[Int] = {
    var globalMax = -1
    for (i <- arr.length - 1 to 0 by -1) {
      val localMax = globalMax max arr(i)
      arr(i) = globalMax
      globalMax = localMax
    }
    arr
  }

  def moveZeroes(nums: Array[Int]): Unit = {
    var (zeroPointer, nonZeroPointer) = (0, 0)

    while (nonZeroPointer < nums.length) {
      if (nums(nonZeroPointer) != 0) {
        val tempVal = nums(zeroPointer)
        nums(zeroPointer) = nums(nonZeroPointer)
        nums(nonZeroPointer) = tempVal
      }
      nonZeroPointer += 1
      if (!(nums(zeroPointer) == 0)) zeroPointer += 1
    }
  }

  def sortArrayByParity(nums: Array[Int]): Array[Int] = {
    var (start, end) = (0, 0)

    while (end < nums.length) {
      if (nums(end) % 2 == 0) {
        val tempVal = nums(start)
        nums(start) = nums(end)
        nums(end) = tempVal
      }
      end += 1
      if (nums(start) % 2 == 0) {
        start += 1
      }
    }
    nums
  }

  def heightChecker(heights: Array[Int]): Int = {
    heights.sorted.zip(heights).count(t => t._1 != t._2)
  }

  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    var (globalMax, left, right, i) = (0, 0, 0, 0)

    while (i < nums.length) {
      if (nums(i) == 0) {
        left = right
        right = 0
      } else {
        right += 1
      }
      globalMax = globalMax max (left + right + 1)
      i += 1
    }
    globalMax min nums.length
  }

  def thirdMax(nums: Array[Int]): Int = {
    val trie: Array[Long] = Array.fill(3)(Long.MinValue)

    for (elem <- nums) {
      var keep: Long = elem
      for (i <- 0 to 2) {
        if (keep > trie(i)) {
          val newKeep = trie(i)
          trie(i) = keep
          keep = newKeep
        } else if (keep == trie(i)) {
          keep = Long.MinValue
        }
      }
    }
    if (trie(2) != Long.MinValue) trie(2).toInt else trie(0).toInt
  }

  def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
    var i = 0
    val res: ListBuffer[Int] = ListBuffer()

    while (i < nums.length) {
      (nums(i), nums(nums(i) - 1)) match {
        case (a, b) if a == i + 1 || a == b => i += 1
        case _ => val temp = nums(i); nums(i) = nums(nums(i) - 1); nums(temp - 1) = temp
      }
    }

    for (i <- nums.indices) {
      if (nums(i) != i + 1) {
        res += i + 1
      }
    }

    res.toList
  }

  def sortedSquares(nums: Array[Int]): Array[Int] = {
    val n = nums.length
    var i = 0
    while (i < n && nums(i) < 0) {
      i += 1
    }

    var (left, right) = (i - 1, i)
    val res: ListBuffer[Int] = ListBuffer()

    while (right - left < n + 1) {
      val leftSq = if (left >= 0) Math.pow(nums(left), 2).toInt else Int.MaxValue
      val rightSq = if (right < n) Math.pow(nums(right), 2).toInt else Int.MaxValue
      if (leftSq < rightSq) {
        res += leftSq
        left -= 1
      } else {
        res += rightSq
        right += 1
      }
    }
    res.toArray
  }

  def reverseString(s: Array[Char]): Unit = {
    val n = s.length
    var i = 0

    while (i < n / 2) {
      val tempVal = s(i)
      s(i) = s(n - i - 1)
      s(n - i - 1) = tempVal
      i += 1
    }
  }

  def validPalindromeTwo(s: String): Boolean = {
    var (start, end) = (0, s.length - 1)

    while (start < end && s(start) == s(end)) {
      start += 1
      end -= 1
    }

    var leftShift = start + 1
    var rightShift = end - 1

    var firstBool = true
    while (leftShift < end & firstBool) {
      firstBool = if (s(leftShift) != s(end)) false else true
      leftShift += 1
      end -= 1
    }

    var secondBool = true
    while (start < rightShift & secondBool) {
      secondBool = if (s(start) != s(rightShift)) false else true
      start += 1
      rightShift -= 1
    }

    firstBool || secondBool
  }

  def nextPermutation(nums: Array[Int]): Unit = {

    if (nums.length == 1) return

    def swap(leftIndex: Int, rightIndex: Int): Unit = {
      val tempVal = nums(leftIndex)
      nums(leftIndex) = nums(rightIndex)
      nums(rightIndex) = tempVal
    }

    def insertionSort(startFrom: Int): Unit = {
      var i = startFrom
      while (i < nums.length) {
        var thisI = i
        while (thisI != startFrom && nums(thisI) < nums(thisI - 1)) {
          swap(thisI, thisI - 1)
          thisI -= 1
        }
        i += 1
      }
    }

    var (leftPointer, rightPointer) = (nums.length - 2, nums.length - 1)

    while (leftPointer != 0 && nums(leftPointer) >= nums(leftPointer + 1)) {
      leftPointer -= 1
    }

    while (rightPointer != 0 && nums(leftPointer) >= nums(rightPointer)) {
      rightPointer -= 1
    }

    swap(leftPointer, rightPointer)

    insertionSort(leftPointer + (if (leftPointer == rightPointer) 0 else 1))

  }

  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    val nums1Copy: Array[Int] = nums1.clone()
    var (writePointer, readPointer1, readPointer2) = (n + m - 1, m - 1, n - 1)

    while (writePointer >= 0) {
      if (readPointer1 >= 0 && (readPointer2 < 0 || nums1Copy(readPointer1) >= nums2(readPointer2))) {
        nums1(writePointer) = nums1Copy(readPointer1)
        readPointer1 -= 1
      } else {
        nums1(writePointer) = nums2(readPointer2)
        readPointer2 -= 1
      }
      writePointer -= 1
    }
  }


  def expand(s: String): Array[String] = {

    @tailrec
    def splitter(i: Int, output: Array[String], isBraceOpen: Boolean): Array[String] = {
      if (i == s.length) output
      else if (s(i) == '{') splitter(i + 1, output :+ "", isBraceOpen = true)
      else if (s(i) == '}') splitter(i + 1, output, isBraceOpen = false)
      else if (s(i) == ',') splitter(i + 1, output, isBraceOpen)
      else if (isBraceOpen && s(i) != ',') splitter(i + 1, output.dropRight(1) :+ output.last + s(i), isBraceOpen)
      else splitter(i + 1, output :+ s(i).toString, isBraceOpen)
    }

    val splitted = splitter(0, Array(), isBraceOpen = false)
    val res: ListBuffer[String] = ListBuffer()

    def rec(i: Int, substr: String): Unit = {
      if (i == splitted.length) res += substr
      else for (letter <- splitted(i).sorted if letter != ',') rec(i + 1, substr + letter)
    }

    rec(0, "")
    res.toArray

  }

  def shiftGrid(grid: Array[Array[Int]], k: Int): List[List[Int]] = {
    val (m, n) = (grid.length, grid(0).length)
    val matrixDim = m * n
    val divK = k - (k / matrixDim) * matrixDim
    (grid.flatten.takeRight(divK) ++ grid.flatten.take(matrixDim - divK)).grouped(n).map(_.toList).toList
  }

  def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    var hm1 = nums1.groupBy(identity).map(x => (x._1, x._2.length))
    var res: Array[Int] = Array()

    for (elem <- nums2) {
      if (hm1.contains(elem)) {
        res = res :+ elem
        hm1 = hm1 + (elem -> hm1(elem).-(1))
        if (hm1(elem) == 0) hm1 -= elem
      }
    }
    res
  }

  def countPrefixes(words: Array[String], s: String): Int = {
    var c = 0
    for (word <- words) {
      val n = word.length
      c += (if (s.take(n) == word) 1 else 0)
    }
    c
  }

  def backspaceCompareNaive(s: String, t: String): Boolean = {
    var left: List[Char] = List()
    var right: List[Char] = List()

    for (i <- 0 until (s.length max t.length)) {
      if (i < s.length && s(i) == '#' && left.nonEmpty) left = left.tail else if (i < s.length && s(i) != '#') left = s(i) +: left
      if (i < t.length && t(i) == '#' && right.nonEmpty) right = right.tail else if (i < t.length && t(i) != '#') right = t(i) +: right
    }
    left.mkString("") == right.mkString("")
  }

  def findUnsortedSubarray(nums: Array[Int]): Int = {
    /** Sorting approach
     * Time complexity: O (n log n)
     * Space complexity: O(n)
     * */
    val a = nums.zip(nums.sorted)
    val f = a.indexWhere(x => x._1 != x._2)
    val l = a.lastIndexWhere(x => x._1 != x._2)
    if (f == -1) 0 else l - f + 1
  }

  def findPermutation(s: String): Array[Int] = {
    val res = Array.fill(s.length + 1)(0)
    var (m, skipped) = (1, 0)
    for (c <- s.zipWithIndex) {
      if (c._1 == 'D') {
        skipped += 1
      } else {
        res(c._2) = m
        while (skipped != 0) {
          res(c._2 - skipped) = m + skipped
          skipped -= 1
        }
        m = c._2 + 2
      }
    }
    while (skipped != 0) {
      res(s.length - skipped) = m + skipped
      skipped -= 1
    }
    res(s.length) = m
    res
  }

  def removeDuplicates(s: String, k: Int): String = {
    var stack: List[(Char, Int)] = List((' ', 0))
    var i = 0

    while (i < s.length) {
      var headElem = stack.head
      stack = stack.tail

      if (headElem._1 == s(i)) {
        headElem = (headElem._1, headElem._2 + 1)
        if (headElem._2 < k) stack = headElem +: stack
      } else {
        stack = headElem +: stack
        stack = (s(i), 1) +: stack
      }
      i += 1
    }

    stack.foldRight("")((y, x) => x + y._1.toString * y._2)

  }

  def find132pattern(nums: Array[Int]): Boolean = {
    val leftMin = nums.scanLeft(Int.MaxValue)(_ min _).dropRight(1)
    var stack = List(nums(nums.length - 1))
    var flag = false
    var j = nums.length - 2
    while (!flag && j > 0) {
      if (nums(j) > leftMin(j)) {
        if (nums(j) < stack.head) stack = nums(j) +: stack
        else {
          while (!flag && stack.nonEmpty && nums(j) > stack.head) {
            val kElem = stack.head
            stack = stack.tail
            if (leftMin(j) < kElem) flag = true
          }
          stack = nums(j) +: stack
        }
      }
      j -= 1
    }
    flag

  }

  def largestGoodInteger(num: String): String = {
    var res: ArrayBuffer[Char] = ArrayBuffer()
    var maxVal: Int = -1
    for (letter <- num.reverse) {
      if (res.isEmpty || letter == res.head) res += letter
      else res = ArrayBuffer(letter)

      if (res.length == 3) maxVal = res.mkString("").toInt max maxVal
    }

    if (res.length == 3) maxVal = res.mkString("").toInt max maxVal

    if (maxVal == -1) ""
    else if (maxVal == 0) "000"
    else maxVal.toString
  }

  def letterCombinations(digits: String): List[String] = {
    def charToButton(c: Char): Int = {
      val factor = if (c >= 's' && c != 'z') 1 else if (c == 'z') 2 else 0
      (c.toInt - 97 - factor) / 3 + 2
    }

    val mp = ('a' to 'z').foldLeft(Map[Int, String]())(
      (map, c) => {
        val button = charToButton(c)
        map.updated(button, map.getOrElse(button, "") + c)
      }
    )

    var res: List[String] = List()
    var stack: List[Int] = digits.reverse.map(x => mp(x.asDigit).length - 1).toList

    while (stack.nonEmpty) {
      if (stack.length == digits.length)
        res =
          stack
            .reverse
            .zipWithIndex
            .map(x => mp(digits(x._2).asDigit)(x._1))
            .mkString("") +: res
      val h = stack.head
      stack = stack.tail
      if (h != 0) {
        stack = (h - 1) +: stack
        while (stack.length < digits.length) stack = (mp(digits(stack.length).asDigit).length - 1) +: stack
      }
    }
    res
  }

  def combinationSum3(k: Int, n: Int): List[List[Int]] = {
    var res: List[List[Int]] = List()
    var stack: List[Int] = (1 to k).toList

    while (stack.nonEmpty && stack.head < n / k) {
      if (stack.length == k && stack.sum == n) res = res :+ stack
      val h = stack.last
      stack = stack.dropRight(1)
      if (h != 9) {
        stack = stack :+ h + 1
        while (stack.length < k && stack.last < 9) stack = stack :+ (stack.last + 1)
      }
    }
    res
  }

  def countVowelStrings(n: Int): Int = {
    val counts = Array.fill(5)(1)
    for (_ <- 1 until n; j <- 3 to 0 by -1) counts(j) = counts(j) + counts(j + 1)
    counts.sum
  }

  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).drop(1)
  }

  def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val m = matrix.length // rows number
    val n = matrix(0).length // columns number
    val newMatrix = Array.fill(n)(Array.fill(m)(0))

    for (i <- 0 until (m min n)) {
      newMatrix(i)(i) = matrix(i)(i)
      for (j <- i + 1 until n) {
        newMatrix(j)(i) = matrix(i)(j)
      }
      for (j <- i + 1 until m) {
        newMatrix(i)(j) = matrix(j)(i)
      }
    }
    newMatrix
  }

}