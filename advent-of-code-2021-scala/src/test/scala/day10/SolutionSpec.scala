package day10

import zio.test._
import Assertion._
import common.ZFileReader
import day10.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day10.SolutionSpec")(
    test("getErrorCharacter returns correct error for line") {
      assert(getErrorCharacter("{([(<{}[<>[]}>{[]{[(<()>"))(equalTo(Some('}'))) &&
      assert(getErrorCharacter("[[<[([]))<([[{}[[()]]]"))(equalTo(Some(')'))) &&
      assert(getErrorCharacter("[{[{({}]{}}([{[{{{}}([]"))(equalTo(Some(']'))) &&
      assert(getErrorCharacter("[<(<(<(<{}))><([]([]()"))(equalTo(Some(')'))) &&
      assert(getErrorCharacter("<{([([[(<>()){}]>(<<{{"))(equalTo(Some('>')))
    },
    test("getErrorScore should get correct score from test input") {
      for {
        input <- ZFileReader.readLines("day10-test-input.txt")(parse)
      } yield assert(getErrorScore(input))(equalTo(26397))
    },
    test("getErrorScore should get correct score from real input") {
      for {
        input <- ZFileReader.readLines("day-10-input-william.txt")(parse)
      } yield assert(getErrorScore(input))(equalTo(268845))
    },
    test("getCompletionScore should return correct completionScore") {
      assert(getCompletionScore("}}]])})]".toSeq))(equalTo(288957L)) &&
      assert(getCompletionScore(")}>]})".toSeq))(equalTo(5566L)) &&
      assert(getCompletionScore("}}>}>))))".toSeq))(equalTo(1480781L)) &&
      assert(getCompletionScore("]]}}]}]}>".toSeq))(equalTo(995444L)) &&
      assert(getCompletionScore("])}>".toSeq))(equalTo(294L))
    },
    test("getMiddleCompletionScore should return correct score for test input"){
      for {
        input <- ZFileReader.readLines("day10-test-input.txt")(parse)
      } yield assert(getMiddleCompletionScore(input))(equalTo(288957L))
    },
    test("getMiddleCompletionScore should return correct score for real input") {
      for {
        input <- ZFileReader.readLines("day-10-input-william.txt")(parse)
      } yield assert(getMiddleCompletionScore(input))(equalTo(4038824534L))
    }
  )
}
