package day15

import zio.test._
import Assertion._
import common.ZFileReader
import day15.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day15.SolutionSpec")(
    test("getLowestRisk should return lowest total risk for test input") {
      for {
        rawInput <- ZFileReader.readLines("day15-test-input.txt")(parse)
      } yield assert(getLowestRisk(rawInput))(equalTo(40))
    },
    test("getLowestRisk should return lowest total risk for real input") {
      for {
        rawInput <- ZFileReader.readLines("day-15-input-william.txt")(parse)
      } yield assert(getLowestRisk(rawInput))(equalTo(441))
    },
    test("getLowestRisk should return lowest total risk for expanded test input") {
      for {
        rawInput <- ZFileReader.readLines("day15-test-input.txt")(parse)
      } yield assert(getLowestRisk(expand(rawInput)))(equalTo(315))
    },
    test("getLowestRisk should return lowest total risk for expanded real input") {
      for {
        rawInput <- ZFileReader.readLines("day-15-input-william.txt")(parse)
      } yield assert(getLowestRisk(expand(rawInput)))(equalTo(2849))
    }
  )
}
