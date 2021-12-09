package day9

object Solution {

  def parse(line: String): Seq[Int] = line.map(_.toInt - 48)

  def processInput(rawInput: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val finalRowSize = rawInput(0).size + 2
    Seq(Seq.fill(finalRowSize)(Int.MaxValue)) :++
      rawInput.map(r => Int.MaxValue +: r :+ Int.MaxValue) :++
      Seq(Seq.fill(finalRowSize)(Int.MaxValue))
  }

  def getRiskLevel(input: Seq[Seq[Int]]): Int =
    getLowestPoints(input).map(p => input(p._1)(p._2) + 1).sum

  def getLowestPoints(input: Seq[Seq[Int]]): Seq[(Int, Int)] = (
    for {
      r <- 1 until input.size - 1
      c <- 1 until input(0).size - 1
    } yield
      if (
        input(r - 1)(c) > input(r)(c) &&
        input(r + 1)(c) > input(r)(c) &&
        input(r)(c - 1) > input(r)(c) &&
        input(r)(c + 1) > input(r)(c)
      )
        Seq((r, c))
      else
        Seq.empty
  ).flatten
}
