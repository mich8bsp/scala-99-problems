import scala.io.StdIn

/** Find the last element of a list.
  *
  */

object Problem01 {

  def last(nums: List[Int]): Int = nums.last

  def main(args: Array[String]): Unit = {
    val inputList: List[Int] =  StdIn.readLine()
      .split(" ")
      .map(_.toInt)
      .toList

    val res = last(inputList)
    print(res)
  }
}
