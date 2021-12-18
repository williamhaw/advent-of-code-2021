package day11

import scala.annotation.tailrec

object Solution {

  def parse(line: String): Seq[Int] = line.strip().map(_.asDigit)

  def toSeqSeq(grid: Array[Array[Int]]): Seq[Seq[Int]] = grid.map(_.toSeq).toSeq

  val directions = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  def getNeighbours(point: (Int, Int), grid: Seq[Seq[Int]]): Set[(Int, Int)] =
    directions
      .map { case (dx, dy) => (point._1 + dx, point._2 + dy) }
      .filter { case (r, c) => grid.indices.contains(r) && grid(0).indices.contains(c) }
      .toSet

  def incrementLevel(grid: Seq[Seq[Int]]): Seq[Seq[Int]] =
    grid.map(row => row.map(ele => ele + 1))

  @tailrec
  def getFlashed(grid: Seq[Seq[Int]], flashed: Set[(Int, Int)]): Set[(Int, Int)] = {
    val next = flashed ++
      flashed
        .flatMap(p => getNeighbours(p, grid))
        .filter(candidate =>
          grid(candidate._1)(candidate._2) + flashed.intersect(getNeighbours(candidate, grid)).size > 9
        )
    if (next == flashed) flashed else getFlashed(grid, next)
  }

  def step(grid: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val incr = incrementLevel(grid)
    val initialFlash = (for { r <- grid.indices; c <- grid(0).indices } yield (r, c)).filter { case (r, c) =>
      incr(r)(c) > 9
    }.toSet
    val flashed = getFlashed(incr, initialFlash)
    incr.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (ele, c) =>
        if (flashed.contains((r, c))) 0 else ele + getNeighbours((r, c), incr).count(flashed.contains)
      }
    }
  }

  @tailrec
  def stepN(n: Int, grid: Seq[Seq[Int]]): Seq[Seq[Int]] =
    if (n > 0)
      stepN(n - 1, step(grid))
    else
      grid

  @tailrec
  def stepNTotalFlashes(n: Int, grid: Seq[Seq[Int]], count: Int): Int =
    if (n > 0) {
      val output = step(grid)
      stepNTotalFlashes(n - 1, output, count + output.map(_.count(_ == 0)).sum)
    } else
      count

}
