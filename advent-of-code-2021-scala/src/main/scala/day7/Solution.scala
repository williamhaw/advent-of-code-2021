package day7

object Solution {

  def parse(line: String): List[Int] = line.split(",").map(_.toInt).toList

  def getLowestTotalFuelCost(crabPositions: List[Int], distanceToCost: Int => Int): Int = {
    val maxPosition = crabPositions.max
    val positions   = Vector.tabulate(maxPosition + 1)(i => crabPositions.count(_ == i))

    val costs = for {
      costIdx <- 0 until positions.size
    } yield {
      for {
        positionIdx <- 0 until positions.size
      } yield positions(positionIdx) * distanceToCost((positionIdx - costIdx).abs)
    }.sum

    costs.min
  }

  def getLowestTotalFuelCost(crabPositions: List[Int]): Int =
    getLowestTotalFuelCost(crabPositions, identity)

  def getLowestTotalFuelCostArithmeticProgression(crabPositions: List[Int]): Int =
    getLowestTotalFuelCost(crabPositions, (d: Int) => (d * (2 + (d - 1))) / 2)

}
