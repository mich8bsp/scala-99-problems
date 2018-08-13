import scala.io.StdIn

object ListProblems1to28 {


/** Problem 1: Find the last element of a list.
  *
  */
  def last(nums: List[Int]): Int = {
    if (nums.nonEmpty){
      nums.last
    }else{
      throw new IllegalArgumentException("Should provide a list with at least 1 member")
    }
  }

  /** Problem 2: Find the last but one element of a list.
    *
    */
  def penultimate(nums: List[Int]): Int = {
    if (nums.length >= 2){
      nums(nums.length-2)
    }else{
      throw new IllegalArgumentException("Should provide a list with at least 2 members")
    }
  }

  /** Problem 3: Find the kth element of a list.
    *
    */
  def nth(idx: Int, nums: List[Int]): Int ={
    if(nums.length >= idx){
      nums(idx-1)
    }else{
      throw new IllegalArgumentException("The provided list was too small")
    }
  }

  def main(args: Array[String]): Unit = {
    val inputList: List[Int] =  StdIn.readLine()
      .split(" ")
      .map(_.toInt)
      .toList

    var res = last(inputList)
    print(res)

    res = penultimate(inputList)
    print(res)

    res = nth(inputList.head, inputList.tail)
    print(res)
  }
}
