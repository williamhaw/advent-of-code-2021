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
        } yield assert(getRiskLevel(processInput(input)))(equalTo(15))
    }
  )
}
