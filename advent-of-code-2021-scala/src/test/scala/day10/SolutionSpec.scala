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
    }
  )
}
