package day9

import zio.test._
import Assertion._
import common.ZFileReader
import day9.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day9.SolutionSpec")(
    test("getRiskLevel should return correct output for test input") {
      for {
        input <- ZFileReader.readLines("day9-test-input.txt")(parse)
      } yield assert(getRiskLevel(processInput(input)))(equalTo(15))
    },
    test("getRiskLevel should return correct output for real input") {
      for {
        input <- ZFileReader.readLines("day-9-input-william.txt")(parse)
      } yield assert(getRiskLevel(processInput(input)))(equalTo(577))
    },
    test("getLowestPoints should return lowest points for test input") {
      for {
        input <- ZFileReader.readLines("day9-test-input.txt")(parse)
      } yield assert(getLowestPoints(processInput(input)))(
        equalTo(
          Seq(
            (1, 2),
            (1, 10),
            (3, 3),
            (5, 7)
          )
        )
      )
    },
    test("getBasinSize should return correct basin size for test input") {
      for {
        input <- ZFileReader.readLines("day9-test-input.txt")(parse)
      } yield assert(getBasinSize(processInput(input), (1,2)))(equalTo(3))
    },
    test("getThreeLargestBasinSizes should return correct product of basin sizes for test input") {
      for {
        input <- ZFileReader.readLines("day9-test-input.txt")(parse)
      } yield assert(getThreeLargestBasinSizes(processInput(input)))(equalTo(1134))
    },
    test("getThreeLargestBasinSizes should return correct product of basin sizes for real input") {
      for {
        input <- ZFileReader.readLines("day-9-input-william.txt")(parse)
      } yield assert(getThreeLargestBasinSizes(processInput(input)))(equalTo(1069200))
    },
  )
}
