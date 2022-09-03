object TwoSum {
  //  https://leetcode.com/problems/two-sum/


  object Solution {
    //A really brute force way would be to search for all possible pairs of numbers
    //Runtime: 1549 ms, faster than 9.02% of Scala online submissions for Two Sum.
    //Memory Usage: 319.6 MB, less than 6.66% of Scala online submissions for Two Sum.
    /*    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
          (for {
            a <- nums.indices
            b <- a + 1 until nums.length
            if nums(a) + nums(b) == target
          } yield Array(a, b)).head
        }*/


    //        Runtime: 907 ms, faster than 71.10% of Scala online submissions for Two Sum.
    //        Memory Usage: 86.1 MB, less than 17.49% of Scala online submissions for Two Sum.
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      def loop(index: Int, map: Map[Int, Int] = Map()): Array[Int] = {
        map.get(target - nums(index)) match {
          case None => loop(index + 1, map ++ Map(nums(index) -> index))
          case Some(previousIndex) => Array(previousIndex, index)
        }
      }

      loop(0)
    }
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.twoSum(Array(3, 2, 3), 6) sameElements Array(0, 2))
    assert(Solution.twoSum(Array(2, 7, 11, 15, 7), 9) sameElements Array(0, 1))
    assert(Solution.twoSum(Array(3, 2, 4), 6) sameElements Array(1, 2))
    assert(Solution.twoSum(Array(3, 3), 6) sameElements Array(0, 1))
  }
}
