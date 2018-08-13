import scala.io.StdIn

/** Find the last but one element of a list.
  *
  */

object Problem02 {
  def penultimate(nums: List[Int]): Int = {
    if (nums.length >= 2){
      nums(nums.length-2)
    }else{
      throw new IllegalArgumentException("Should provide a list with at least 2 members")
    }
  }

  def main(args: Array[String]): Unit = {
    val inputList: List[Int] =  StdIn.readLine()
      .split(" ")
      .map(_.toInt)
      .toList

    val res = penultimate(inputList)
    print(res)
  }
}
