package day3

import zio.test._
import Assertion._
import common.ZFileReader
import day3.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day3.SolutionSpec")(
    test("getGammaRate should give 22 given input") {
      for {
        input <- ZFileReader.readLines("day3-test-input.txt")(parse)
      } yield assert(getGammaRate(input))(equalTo(22))
    },
    test("getEpsilonRate should give 9 given input") {
      for {
        input <- ZFileReader.readLines("day3-test-input.txt")(parse)
      } yield assert(getEpsilonRate(input))(equalTo(9))
    },
    test("getGammaRate should give correct rate for real input"){
        for {
        input <- ZFileReader.readLines("day-3-input-william.txt")(parse)
      } yield assert(getGammaRate(input))(equalTo(3349))
    },
    test("getEpsilonRate should give correct rate for real input"){
        for {
        input <- ZFileReader.readLines("day-3-input-william.txt")(parse)
      } yield assert(getEpsilonRate(input))(equalTo(746))
    }
  )
}
