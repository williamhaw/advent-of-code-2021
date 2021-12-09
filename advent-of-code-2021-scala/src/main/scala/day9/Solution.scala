package day9

object Solution {

  def parse(line: String): Seq[Int] = line.map(_.toInt - 48)

  def processInput(rawInput: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val finalRowSize = rawInput(0).size + 2
    Seq(Seq.fill(finalRowSize)(Int.MaxValue)) :++
      rawInput.map(r => Int.MaxValue +: r :+ Int.MaxValue) :++
      Seq(Seq.fill(finalRowSize)(Int.MaxValue))
  }

  def getRiskLevel(input: Seq[Seq[Int]]): Int = {
    val levels = for {
      r <- 1 until input.size - 1
      c <- 1 until input(0).size - 1
    } yield {
      val currentHeight = input(r)(c)
      if (
        input(r - 1)(c) > currentHeight &&
        input(r + 1)(c) > currentHeight &&
        input(r)(c - 1) > currentHeight &&
        input(r)(c + 1) > currentHeight
      )
        currentHeight + 1
      else
        0
    }
    println(levels.filter(_ != 0))
    levels.sum
  }
}
