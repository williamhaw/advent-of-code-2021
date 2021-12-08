package day8

import zio.test._
import Assertion._
import common.ZFileReader
import day8.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day8.SolutionSpec")(
    test("getCountOfOneFourSevenEights should give correct count for test input") {
      for{
          signalOutputs <- ZFileReader.readLines("day8-test-input.txt")(parse)
      } yield assert(getCountOfOneFourSevenEights(signalOutputs))(equalTo(26))
    },
    test("getCountOfOneFourSevenEights should give correct count for real input") {
      for{
          signalOutputs <- ZFileReader.readLines("day-8-input-william.txt")(parse)
      } yield assert(getCountOfOneFourSevenEights(signalOutputs))(equalTo(255))
    }
  )
}
