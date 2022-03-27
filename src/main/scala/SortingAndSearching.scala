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


  def tripletBinarySearch(x: Array[(Int, Int, Int)], toFind: Int): Int = {
    if (toFind >= x.last._2) {
      return x.length - 1
    }

    var (left, middle, right) = (0, (x.length - 1) / 2, x.length - 1)
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

}

