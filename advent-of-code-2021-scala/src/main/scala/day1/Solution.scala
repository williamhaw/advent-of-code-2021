package day1

object Solution {

  case class SumAndPrevious(sum: Int, previous: Int)

  def countIncrease(input: List[Int]): Int = {
    val result = input.foldLeft(SumAndPrevious(0, 0))((sp: SumAndPrevious, current: Int) =>
      if (current > sp.previous) sp.copy(sum = sp.sum + 1, previous = current) else sp.copy(previous = current)
    )
    result.sum - 1
  }

  def countIncreaseWithWindow(input: List[Int], window: Int) = countIncrease(input.sliding(window, 1).map(_.sum).toList)

}
