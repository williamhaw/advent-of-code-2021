package day15

import scala.annotation.tailrec

object Solution {

  def parse(line: String): Seq[Int] = line.map(_.toInt - 48)

  def getLowestRisk(input: Seq[Seq[Int]]): Int = {
    val start = (0, 0)
    val end   = (input.size - 1, input(0).size - 1)

    val directions = Seq((-1, 0), (0, -1), (1, 0), (0, 1))

    @tailrec
    def helper(risks: Map[(Int, Int), Int], queue: Seq[(Int, Int)]): Map[(Int, Int), Int] =
      if (queue.isEmpty)
        risks
      else {
        val current                  = queue.head
        val (currentRow, currentCol) = current

        val neighbours = directions
          .map { case (dr, dc) => (currentRow + dr, currentCol + dc) }
          .filterNot { case (r, c) => r < 0 || c < 0 || r > end._1 || c > end._2 }

        val newRisk: Map[(Int, Int), Int] = neighbours.collect {
          case next @ (nextRow, nextCol)
              if !risks.contains(next) || risks(current) + input(nextRow)(nextCol) < risks(next) =>
            next -> (risks(current) + input(nextRow)(nextCol))
        }.toMap

        helper(risks ++ newRisk, queue.tail :++ newRisk.keySet)
      }

    helper(Map(start -> 0), Seq(start))(end)
  }

  def expand(input: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val end             = (input.size - 1, input(0).size - 1)
    val (width, height) = (input.size, input(0).size)

    Seq.tabulate(width * 5, height * 5) { case (absRow, absCol) =>
      val (inputRow, inputCol) = (absRow % width, absCol % height)
      val (tileRow, tileCol)   = (absRow / width, absCol / height)
      1 + (input(inputRow)(inputCol) - 1 + tileRow + tileCol) % 9
    }
  }

}
