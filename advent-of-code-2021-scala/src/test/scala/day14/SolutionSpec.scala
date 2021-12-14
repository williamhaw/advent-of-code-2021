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
    }
  )
}
