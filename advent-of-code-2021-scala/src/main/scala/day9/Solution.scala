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

  def getBasinSize(input: Seq[Seq[Int]], lowestPoint: (Int, Int)): Int = {

    def helper(currentPoint: (Int, Int), seen: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (seen.contains(currentPoint))
        return seen
      else {
        val (r, c)      = currentPoint
        val currentSeen = seen + currentPoint
        val nextPoints  = List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)).filter(p => input(p._1)(p._2) < 9)
        nextPoints.foldLeft(currentSeen)((b, a) => helper(a, b))
      }
    }

    helper(lowestPoint, Set.empty).size

  }

  def getThreeLargestBasinSizes(input: Seq[Seq[Int]]): Int =
    getLowestPoints(input).map(p => getBasinSize(input, p)).sorted.takeRight(3).product
}
