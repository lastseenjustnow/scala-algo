import java.security.KeyStore.TrustedCertificateEntry

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

  def findKthPositive(arr: Array[Int], k: Int): Int = {
    var (res, i) = (k, 0)
    while (i < arr.length && arr(i) <= res) {
      res += 1
      i += 1
    }
    res
  }

}

