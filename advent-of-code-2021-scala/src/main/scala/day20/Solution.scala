package day20

import scala.annotation.tailrec

object Solution {

  def parse(line: String): Seq[Char] = line.strip().toCharArray()

  def processFloorSet(input: Seq[Seq[Char]]): Set[(Int, Int)] =
    (for {
      r <- input.indices
      c <- input(0).indices
      if input(r)(c) == '#'
    } yield (r, c)).toSet

  def parseAlgorithm(line: String): Seq[Boolean] = line.strip().map {
    case '#' => true
    case '.' => false
  }

  def getIndex(
      floor: Set[(Int, Int)],
      point: (Int, Int),
      topLeft: (Int, Int),
      bottomRight: (Int, Int),
      flipBit: Boolean
  ): Int = {
    val (row, col)       = point
    val (minRow, minCol) = topLeft
    val (maxRow, maxCol) = bottomRight
    var index            = 0
    for {
      r <- (row - 1) to (row + 1)
      c <- (col - 1) to (col + 1)
    } yield {
      index <<= 1
      index = index | (if (floor.contains((r, c))) 1 else 0)
      index = index | (if (flipBit && (r < minRow || r > maxRow || c < minCol || c > maxCol)) 1 else 0)
    }
    index
  }

  def step(floor: Set[(Int, Int)], algorithm: Seq[Boolean], flipBit: Boolean): Set[(Int, Int)] = {
    val (minRow, minColumn) = (floor.map(_._1).min, floor.map(_._2).min)
    val (maxRow, maxColumn) = (floor.map(_._1).max, floor.map(_._2).max)

    (
      for {
        r <- (minRow - 1) to (maxRow + 1)
        c <- (minColumn - 1) to (maxColumn + 1)
        if algorithm(getIndex(floor, (r, c), (minRow, minColumn), (maxRow, maxColumn), flipBit))
      } yield (r, c)
    ).toSet
  }

  @tailrec
  def takeNSteps(n: Int, floor: Set[(Int, Int)], algorithm: Seq[Boolean]): Set[(Int, Int)] =
    if (n == 0)
      floor
    else {
      takeNSteps(n - 1, step(floor, algorithm, algorithm(0) && (n % 2 == 1)), algorithm)
    }
}
