package day20

import zio.test._
import Assertion._
import common.ZFileReader
import day20.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day20.SolutionSpec")(
    test("getIndex should return correct value from test input") {
      val input = Set((1, 0), (2, 1))
      assert(getIndex(input, (1, 1), (1, 0), (2, 1), false))(equalTo(34))
    },
    test("takeNSteps should return the correct number of lit pixels from test input") {
      for {
        algorithm <- ZFileReader.readLines("day20-test-input-algorithm.txt")(parseAlgorithm).head
        rawFloor  <- ZFileReader.readLines("day20-test-input-image.txt")(parse)
      } yield assert(takeNSteps(1, processFloorSet(rawFloor), algorithm).size)(equalTo(24)) &&
        assert(takeNSteps(2, processFloorSet(rawFloor), algorithm).size)(equalTo(35)) &&
        assert(takeNSteps(50, processFloorSet(rawFloor), algorithm).size)(equalTo(3351))
    },
    test("takeNSteps should return the correct number of lit pixels from real input") {
      for {
        algorithm <- ZFileReader.readLines("day-20-input-algorithm-william.txt")(parseAlgorithm).head
        rawFloor  <- ZFileReader.readLines("day-20-input-image-william.txt")(parse)
      } yield assert(takeNSteps(2, processFloorSet(rawFloor), algorithm).size)(equalTo(5647)) &&
        assert(takeNSteps(50, processFloorSet(rawFloor), algorithm).size)(equalTo(15653))
    }
  )
}
