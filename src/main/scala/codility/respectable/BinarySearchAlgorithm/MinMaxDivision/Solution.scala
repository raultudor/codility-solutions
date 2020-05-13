package codility.respectable.BinarySearchAlgorithm.MinMaxDivision

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// 100%
// O(N*log(N+M))
// https://app.codility.com/programmers/lessons/14-binary_search_algorithm/min_max_division/
// Test results: https://app.codility.com/demo/results/trainingB6CWRA-XSE/
object Solution {

  /**
   * @param k - the max number of blocks
   * @param m - max value of a: M
   * @param a - N integers in range 0..M
   * @return
   */
  def solution(k: Int, m: Int, a: Array[Int]): Int =
    minLargestSum(k, a)

  def minLargestSum(k: Int, a: Array[Int]): Int = {
    var lowerBound = a.foldLeft(0)(Math.max)
    var upperBound = a.sum

    if (k == 1) upperBound
    else if (k > a.length) lowerBound
    else {
      while (lowerBound <= upperBound) {
        val targetSum = (lowerBound + upperBound) / 2 // binary search of the smallest sum
        if (blockSizeValid(targetSum, a, k)) { // criteria: can segments be created having this max sum?
          upperBound = targetSum - 1 // yes => max sum too large
        }
        else
          lowerBound = targetSum + 1 // no => need to increase target max sum
      }
      lowerBound
    }
  }

  def blockSizeValid(maxSum: Int, a: Array[Int], maxSegments: Int): Boolean = {
    var blockSum = 0
    var blockCnt = 1

    for (i <- Stream.range(0, a.length)) {
      if (blockSum + a(i) > maxSum) {
        blockSum = a(i)
        blockCnt += 1
      }
      else {
        blockSum += a(i)
      }
    }

    blockCnt <= maxSegments
  }
}

class SolutionTest extends AnyFlatSpec with Matchers {

  "solution" should "be correct" in {
    Solution.solution(3, 5, Array(2, 1, 5, 1, 2, 2, 2)) should be(6)
    Solution.solution(2, 1, Array(1, 1, 1, 1, 1)) should be(3)
    Solution.solution(3, 1, Array(1, 1, 1, 1, 1)) should be(2)
    Solution.solution(1, 1, Array(1, 1, 1, 1, 1)) should be(5)
  }
}
