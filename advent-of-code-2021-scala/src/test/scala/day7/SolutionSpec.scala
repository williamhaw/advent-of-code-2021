package day7

import zio.test._
import Assertion._
import common.ZFileReader
import day7.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day7.SolutionSpec")(
    test("getLowestTotalFuelCost should get the lowest cost for the test input") {
      val testInput = "16,1,2,0,4,2,7,1,2,14"
      assert(getLowestTotalFuelCost(parse(testInput)))(equalTo(37))
    },
    test("getLowestTotalFuelCost should get the lowest cost for the real input") {
      for {
        input <- ZFileReader.readLines("day-7-input-william.txt")(parse).head
      } yield assert(getLowestTotalFuelCost(input))(equalTo(357353))
    },
    test("getLowestTotalFuelCostArithmeticProgression should get the lowest cost for the test input") {
      val testInput = "16,1,2,0,4,2,7,1,2,14"
      assert(getLowestTotalFuelCostArithmeticProgression(parse(testInput)))(equalTo(168))
    },
    test("getLowestTotalFuelCostArithmeticProgression should get the lowest cost for the real input") {
      for {
        input <- ZFileReader.readLines("day-7-input-william.txt")(parse).head
      } yield assert(getLowestTotalFuelCostArithmeticProgression(input))(equalTo(104822130))
    }
  )
}
