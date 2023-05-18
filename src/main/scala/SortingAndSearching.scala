import java.security.KeyStore.TrustedCertificateEntry
import scala.annotation.tailrec

object SortingAndSearching {
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    nums.sorted.apply(nums.length - k)
  }

  def search(nums: Array[Int], target: Int): Int = {
    var (left, right) = (0, nums.length - 1)
    var res = -1

    while (left <= right && res == -1) {
      val middle = (right - left) / 2 + left
      if (nums(middle) == target) {
        res = middle
      } else if (target > nums(middle)) {
        left = middle + 1
      } else {
        right = middle - 1
      }
    }
    res
  }

  def searchInsert(nums: Array[Int], target: Int): Int = {
    if (target > nums.last) {
      return nums.length
    }

    var (left, middle, right) = (0, (nums.length - 1) / 2, nums.length - 1)
    var res = -1
    var flag = false

    while (!flag && left <= right) {
      middle = (right - left) / 2 + left
      if (nums(middle) == target) {
        flag = true
        res = middle
      } else if (target < nums(middle)) {
        right = middle - 1
        res = left
      } else {
        left = middle + 1
        res = left
      }
    }
    res
  }

  def searchInsertFirst(nums: Array[Int], target: Int): Int = {

    var (left, middle, right) = (0, 0, nums.length - 1)

    while (left <= right && right != 0) {
      middle = (right - left) / 2 + left
      if (nums(middle) == target) {
        while (middle != 0 && nums(middle - 1) == nums(middle)) {
          middle -= 1
        }
        return middle
      }
      if (target < nums(middle)) {
        right = 0 max (middle - 1)
      } else {
        left = middle + 1
      }
    }
    if (nums(right) < target) right + 1 else right
  }

  def firstBiggerThan(nums: Array[Int], target: Int): Int = {

    var (left, right) = (0, nums.length - 1)

    while (left <= right) {
      val middle = (right - left) / 2 + left
      if (nums(middle) <= target) left = middle + 1
      else right = middle - 1
    }
    left
  }

  def peakIndexInMountainArray(arr: Array[Int]): Int = {
    var (left, right) = (0, arr.length - 1)
    while (true) {
      val mid = (right - left) / 2 + left
      if (arr(mid) > arr(mid - 1) && arr(mid) > arr(mid + 1)) {
        return mid
      }
      if (arr(mid) > arr(mid + 1)) {
        right = mid
      } else left = mid + 1
    }
    -1
  }

  def tripletBinarySearch(x: Array[(Int, Long, Long)], toFind: Long, start: Int): Int = {
    if (toFind >= x.last._2) {
      return x.length - 1
    }

    var (left, middle, right) = (start, (x.length - 1) / 2, x.length - 1)
    var res = -1
    var flag = false

    while (!flag && left <= right) {
      middle = (right - left) / 2 + left
      if (x(middle)._2 == toFind && x(middle + 1)._2 != toFind) {
        flag = true
        res = middle
      } else if (toFind < x(middle)._2) {
        right = middle - 1
        res = right
      } else {
        left = middle + 1
        res = right
      }
    }
    res
  }

  def insertionSort(nums: Array[Int], startFrom: Int): Unit = {
    var i = startFrom
    while (i < nums.length) {
      var thisI = i
      while (thisI != startFrom && nums(thisI) < nums(thisI - 1)) {
        val tempVal = nums(thisI)
        nums(thisI) = nums(thisI - 1)
        nums(thisI - 1) = tempVal
        thisI -= 1
      }
      i += 1
    }
  }

  def isPerfectSquare(num: Int): Boolean = {
    var (left, right): (Long, Long) = (0L, num)
    while (left <= right) {
      val mid: Long = (right - left) / 2 + left
      if (mid * mid == num) return true
      if (mid * mid < num) left = mid + 1 else right = mid - 1
    }
    false
  }

  def findTheDistanceValue(arr1: Array[Int], arr2: Array[Int], d: Int): Int = {
    arr1.map(x => arr2.exists(y => Math.abs(x - y) <= d)).count(x => !x)
  }

  def mySqrt(x: Int): Int = {
    x match {
      case number if number < 2 => number
      case _ =>
        var (left, right): (Long, Long) = (2, (x / 2).toLong)
        while (left <= right) {
          val mid: Long = (right - left) / 2 + left
          val pivot: Long = mid * mid
          if (pivot == x) return mid.toInt
          if (pivot > x) right = mid - 1
          else left = mid + 1
        }
        right.toInt
    }
  }

  def nextGreatestLetter(letters: Array[Char], target: Char): Char = {
    var (left, right) = (0, letters.length - 1)
    while (left <= right) {
      val mid = (right - left) / 2 + left
      if (target.toInt >= letters(mid).toInt) {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    letters(left % letters.length)
  }

  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    var (left, right, mid) = (0, nums.length - 1, -1)

    while (left <= right & mid == -1) {
      val thisMid = (right - left) / 2 + left
      if (nums(thisMid) == target) mid = thisMid
      else if (nums(thisMid) < target) left = thisMid + 1
      else right = thisMid - 1
    }


    if (mid == -1) return Array(-1, -1)
    val res = Array(mid, mid)

    var (leftMid, rightMid) = (mid, mid)
    while (leftMid >= left) {
      val thisMid = (leftMid - left) / 2 + left
      if (nums(thisMid) == target) {
        res(0) = thisMid
        leftMid = thisMid - 1
      }
      else left = thisMid + 1
    }

    while (rightMid <= right) {
      val thisMid = (right - rightMid) / 2 + rightMid
      if (nums(thisMid) == target) {
        res(1) = thisMid
        rightMid = thisMid + 1
      }
      else right = thisMid - 1
    }
    res

  }

  def findRightInterval(intervals: Array[Array[Int]]): Array[Int] = {
    val n = intervals.length
    val sortedIntervals: Array[(Int, Int)] = intervals.zipWithIndex.map( tup => (tup._1(0), tup._2)).sortBy(_._1)
    val res = Array.fill(n)(-1)
    for (i <- 0 until n) {
      var start_j = 0
      var end_j = n - 1
      val end_i = intervals(i)(1)
      while (start_j <= end_j) {
        val middle = (end_j - start_j) / 2 + start_j
        if (sortedIntervals(middle)._1 >= end_i && (middle == 0 || sortedIntervals(middle - 1)._1 < end_i)) {
          res(i) = sortedIntervals(middle)._2
          start_j = end_j + 1
        }
        else if (sortedIntervals(middle)._1 > end_i) end_j = middle - 1
        else start_j = middle + 1
      }
    }
    res
  }

  def findKthPositive(arr: Array[Int], k: Int): Int = {
    var (res, i) = (k, 0)
    while (i < arr.length && arr(i) <= res) {
      res += 1
      i += 1
    }
    res
  }

  def specialArray(nums: Array[Int]): Int = {
    val sortedNums = nums.sorted(Ordering.Int.reverse)
    var i = 0

    for (j <- nums.length to 1 by -1) {
      while (i < nums.length && (sortedNums(i) >= j)) i += 1
      if (j == i) return j
    }
    -1
  }

  def judgeSquareSum(c: Int): Boolean = {
    /** Time complexity: O(sqrt(c) log c)* */
    for (i <- 0 to Math.sqrt(c.toDouble).toInt) {
      if (Math.sqrt(c - i * i) % 1 == 0) return true
    }
    false
  }

  def maxDistanceIterative(nums1: Array[Int], nums2: Array[Int]): Int = {
    val n = nums2.length

    def bs(index: Int): Int = {
      var (l, r) = (index, n - 1)
      var res = 0
      while (l <= r) {
        val mid = (r - l) / 2 + l
        if (nums1(index) <= nums2(mid)) {
          res = mid - index
          l = mid + 1
        } else {
          r = mid - 1
        }
      }
      res
    }

    nums1.zipWithIndex.foldLeft(0)((maxDist, elem) => maxDist max bs(elem._2))
  }

  def maxDistanceFunctional(nums1: Array[Int], nums2: Array[Int]): Int = {
    /**
     * Binary search approach
     * Time complexity: O (n log n)
     * */
    val n = nums2.length

    def bs(index: Int): Int = {
      @tailrec
      def rec(l: Int, r: Int, res: Int): Int = {
        val mid = (r - l) / 2 + l
        (l, r) match {
          case (x, y) if x > y => res
          case (_, y) if nums1(index) <= nums2(mid) => rec(mid + 1, y, mid - index)
          case (x, _) => rec(x, mid - 1, res)
        }
      }

      rec(index, n - 1, 0)
    }

    nums1.zipWithIndex.foldLeft(0)((maxDist, elem) => maxDist max bs(elem._2))
  }

  def maxDistanceTwoPointers(nums1: Array[Int], nums2: Array[Int]): Int = {
    var (i, j, res) = (0, 0, 0)

    while (i < nums1.length) {
      while (j < nums2.length && nums1(i) <= nums2(j)) j += 1
      res = res max j - i - 1
      i += 1
    }
    res
  }

  def check(nums: Array[Int]): Boolean = {
    var c = 0
    var i = 0
    while (c < 2 && i < nums.length) {
      c += (if (nums(i) < nums(i - 1 + (if (i == 0) nums.length else 0))) 1 else 0)
      i += 1
    }
    !(c > 1)
  }

  def searchInRotatedArray(nums: Array[Int], target: Int): Int = {
    val n = nums.length
    var k = 0
    var i = 0

    while (k == 0 && i < nums.length) {
      if (nums(i) < nums(i - 1 + (if (i == 0) nums.length else 0))) k = i
      i += 1
    }

    var (l, r) = (0, n - 1)
    while (l <= r) {
      val unrotatedMid = (r - l) / 2 + l
      val mid = if (unrotatedMid - (n - k) >= 0) unrotatedMid - (n - k) else n + unrotatedMid - (n - k)
      if (nums(mid) == target) return mid
      if (nums(mid) > target) r = unrotatedMid - 1
      else l = unrotatedMid + 1
    }
    -1
  }

  def searchInRotatedArrayRecurisve(nums: Array[Int], target: Int): Int = {
    @tailrec
    def bs(left: Int, right: Int): Int = {
      val middle = (right - left) / 2 + left
      if (right < left) -1
      else if (nums(middle) == target) middle
      else if (nums(middle) > target) bs(left, middle - 1)
      else bs(middle + 1, right)
    }


    @tailrec
    def searchMin(left: Int, right: Int): Int = {
      val middle = (right - left) / 2 + left
      if (left > right) left
      else if (nums(middle) <= nums(right) && (middle == 0 || nums(middle - 1) > nums(middle))) middle
      else if (nums(middle) < nums(right)) searchMin(left, middle - 1)
      else searchMin(middle + 1, right)
    }

    val n = nums.length
    val arrMinIndex = searchMin(0, n - 1)

    if (target == nums(n - 1)) n - 1
    else if (target > nums(n - 1)) bs(0, arrMinIndex - 1)
    else bs(arrMinIndex, n - 1)

  }

  def findMin(nums: Array[Int]): Int = {

    if (nums.head < nums.last || nums.length == 1) return nums.head

    var (l, r) = (0, nums.length - 1)
    while (l <= r) {
      val mid = (r + l) / 2
      if (nums(mid) > nums(mid + 1)) return nums(mid + 1)
      if (nums(mid) < nums(l)) r = mid - 1
      else l = mid + 1
    }
    l
  }

  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    var (leftI, rightI, summa) = (0, 0, 0)
    var minSizeSub = Int.MaxValue
    while (rightI < nums.length) {
      summa += nums(rightI)
      rightI += 1
      while (summa - nums(leftI) >= target) {
        summa -= nums(leftI)
        leftI += 1
      }
      minSizeSub = minSizeSub min (if (summa < target) Int.MaxValue else rightI - leftI)
    }
    if (minSizeSub == Int.MaxValue) 0 else minSizeSub
  }

  def triangleNumber(nums: Array[Int]): Int = {

    val sortedNums = nums.sorted
    val l = nums.length
    var c = 0

    var (left, right) = (0, 1)
    while (right < l) {
      while (left < right) {
        val middle = firstBiggerThan(sortedNums, sortedNums(right) - sortedNums(left))
        if (middle < right) {
          c += right - middle.max(left + 1)
        }
        left += 1
      }
      left = 0
      right += 1
    }
    c
  }

}
