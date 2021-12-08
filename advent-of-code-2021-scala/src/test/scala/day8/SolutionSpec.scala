package day8

import zio.test._
import Assertion._
import common.ZFileReader
import day8.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day8.SolutionSpec")(
    test("getCountOfOneFourSevenEights should give correct count for test input") {
      for {
        signalOutputs <- ZFileReader.readLines("day8-test-input.txt")(parse)
      } yield assert(getCountOfOneFourSevenEights(signalOutputs))(equalTo(26))
    },
    test("getCountOfOneFourSevenEights should give correct count for real input") {
      for {
        signalOutputs <- ZFileReader.readLines("day-8-input-william.txt")(parse)
      } yield assert(getCountOfOneFourSevenEights(signalOutputs))(equalTo(255))
    },
    test("getDigitFromSignal should decode signals for test input") {
      val input = parse("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
      assert(getDigitFromSignal(input.signals))(
        equalTo(
          Map(
            "acedgfb".toSet -> 8,
            "cdfbe".toSet   -> 5,
            "gcdfa".toSet   -> 2,
            "fbcad".toSet   -> 3,
            "dab".toSet     -> 7,
            "cefabd".toSet  -> 9,
            "cdfgeb".toSet  -> 6,
            "eafb".toSet    -> 4,
            "cagedb".toSet  -> 0,
            "ab".toSet      -> 1
          )
        )
      ) && assert(getOutputFromSignal(input.outputDigits, getDigitFromSignal(input.signals)))(equalTo(5353))
    },
    test("getSumOfOutputs should get correct sum for test input"){
        for {
        signalOutputs <- ZFileReader.readLines("day8-test-input.txt")(parse)
      } yield assert(getSumOfOutputs(signalOutputs))(equalTo(61229))
    },
    test("getSumOfOutputs should get correct sum for real input"){
        for {
        signalOutputs <- ZFileReader.readLines("day-8-input-william.txt")(parse)
      } yield assert(getSumOfOutputs(signalOutputs))(equalTo(982158))
    }
  )
}
