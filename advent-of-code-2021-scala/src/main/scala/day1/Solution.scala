package day1

object Solution {

  case class SumAndPrevious(sum: Int, previous: Int)

  def countIncrease(input: List[Int]): Int = {
    val result = input.foldLeft(SumAndPrevious(0, 0))((sp: SumAndPrevious, current: Int) =>
      if (current > sp.previous) SumAndPrevious(sp.sum + 1, current) else sp.copy(previous = current)
    )
    result.sum - 1
  }

  def countIncreaseWithTuple(input: List[Int]): Int = {
    input.foldLeft((-1, 0))((s, c) => if (c > s._2) (s._1 + 1, c) else (s._1, c))._1
  }

  def countIncreaseWithWindow(input: List[Int], window: Int) = countIncrease(input.sliding(window, 1).map(_.sum).toList)

}
