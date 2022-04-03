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
}

