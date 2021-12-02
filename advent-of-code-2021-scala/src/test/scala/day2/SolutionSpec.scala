package day2

import zio.test._
import Assertion._
import common.ZFileReader
import day2.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day2.Solution")(
    test("findPosition should find the correct position") {
      for {
        commands <- ZFileReader.readLines("day-2-input-william.txt")(parseCommand)
      } yield assert(findPosition(commands))(equalTo(PositionDepth(1950, 823)))
    },
    test("findPositionWithAim should find the correct position") {
      for {
        commands <- ZFileReader.readLines("day-2-input-william.txt")(parseCommand)
      } yield assert(findPositionWithAim(commands))(equalTo(PositionDepthAim(1950,864198,823)))
    }
  )
}
