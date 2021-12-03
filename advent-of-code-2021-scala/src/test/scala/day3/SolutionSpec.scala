package day3

import zio.test._
import Assertion._
import common.ZFileReader
import day3.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day3.SolutionSpec")(
    test("getGammaRate should give 22 given test input") {
      for {
        input <- ZFileReader.readLines("day3-test-input.txt")(parse)
      } yield assert(getGammaRate(input))(equalTo(22))
    },
    test("getGammaRate should give correct rate for real input") {
      for {
        input <- ZFileReader.readLines("day-3-input-william.txt")(parse)
      } yield assert(getGammaRate(input))(equalTo(3349))
    },
    test("getEpsilonRate should give 9 given test input") {
      for {
        input <- ZFileReader.readLines("day3-test-input.txt")(parse)
      } yield assert(getEpsilonRate(input))(equalTo(9))
    },
    test("getEpsilonRate should give correct rate for real input") {
      for {
        input <- ZFileReader.readLines("day-3-input-william.txt")(parse)
      } yield assert(getEpsilonRate(input))(equalTo(746))
    },
    test("getOxygenGeneratorRating should give 23 given test input") {
      for {
        input <- ZFileReader.readLines("day3-test-input.txt")(parse)
      } yield assert(getOxygenGeneratorRating(input))(equalTo(23))
    },
    test("getOxygenGeneratorRating should give correct rate for real input") {
      for {
        input <- ZFileReader.readLines("day-3-input-william.txt")(parse)
      } yield assert(getOxygenGeneratorRating(input))(equalTo(3921))
    },
    test("getCO2ScrubberRating should give 10 given test input") {
      for {
        input <- ZFileReader.readLines("day3-test-input.txt")(parse)
      } yield assert(getCO2ScrubberRating(input))(equalTo(10))
    },
    test("getCO2ScrubberRating should give correct rate for real input") {
      for {
        input <- ZFileReader.readLines("day-3-input-william.txt")(parse)
      } yield assert(getCO2ScrubberRating(input))(equalTo(836))
    }
  )
}
