package day14

import zio.test._
import Assertion._
import common.ZFileReader
import day14.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day14.SolutionSpec")(
    test("performStep should return result after one step for test input") {
      for {
        template <- ZFileReader.readLines("day14-test-template.txt")(identity).head
        rules    <- ZFileReader.readLines("day14-test-rules.txt")(parseRules)
      } yield assert(performStep(template, rules.toMap))(equalTo("NCNBCHB"))
    },
    test("performNSteps should return result after n steps for test input") {
      for {
        template <- ZFileReader.readLines("day14-test-template.txt")(identity).head
        rules    <- ZFileReader.readLines("day14-test-rules.txt")(parseRules)
      } yield assert(performNSteps(1, template, rules.toMap))(equalTo("NCNBCHB")) &&
        assert(performNSteps(2, template, rules.toMap))(equalTo("NBCCNBBBCBHCB")) &&
        assert(performNSteps(3, template, rules.toMap))(equalTo("NBBBCNCCNBBNBNBBCHBHHBCHB")) &&
        assert(performNSteps(4, template, rules.toMap))(equalTo("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"))
    },
    test("getScore should return score after 10 steps for test input") {
      for {
        template <- ZFileReader.readLines("day14-test-template.txt")(identity).head
        rules    <- ZFileReader.readLines("day14-test-rules.txt")(parseRules)
      } yield assert(getScore(performNSteps(10, template, rules.toMap)))(equalTo(1588))
    },
    test("getScore should return score after 10 steps for real input") {
      for {
        template <- ZFileReader.readLines("day-14-input-template-william.txt")(identity).head
        rules    <- ZFileReader.readLines("day-14-input-rules-william.txt")(parseRules)
      } yield assert(getScore(performNSteps(10, template, rules.toMap)))(equalTo(2345))
    },
    test("performStepOptimized should return result after one step for test input") {
      for {
        template <- ZFileReader.readLines("day14-test-template.txt")(identity).head
        rules    <- ZFileReader.readLines("day14-test-rules.txt")(parseRules)
      } yield assert(performStepOptimized(processInput(template), rules.toMap))(
        equalTo(Map("NC" -> 1L, "CN" -> 1L, "NB" -> 1L, "BC" -> 1L, "CH" -> 1L, "HB" -> 1L))
      )
    },
    test("getScoreOptimized should return score after 10 steps for test input") {
      for {
        template <- ZFileReader.readLines("day14-test-template.txt")(identity).head
        rules    <- ZFileReader.readLines("day14-test-rules.txt")(parseRules)
      } yield assert(getScoreOptimized(performNStepsOptimized(10, processInput(template), rules.toMap), template))(
        equalTo(1588L)
      )
    },
    test("getScoreOptimized should return score after 10 steps for real input") {
      for {
        template <- ZFileReader.readLines("day-14-input-template-william.txt")(identity).head
        rules    <- ZFileReader.readLines("day-14-input-rules-william.txt")(parseRules)
      } yield assert(getScoreOptimized(performNStepsOptimized(10, processInput(template), rules.toMap), template))(
        equalTo(2345L)
      )
    },
    test("getScoreOptimized should return score after 40 steps for real input") {
      for {
        template <- ZFileReader.readLines("day-14-input-template-william.txt")(identity).head
        rules    <- ZFileReader.readLines("day-14-input-rules-william.txt")(parseRules)
      } yield assert(getScoreOptimized(performNStepsOptimized(40, processInput(template), rules.toMap), template))(
        equalTo(2432786807053L)
      )
    }
  )
}
