package day21

import zio.test._
import Assertion._
import common.ZFileReader
import day21.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day21.SolutionSpec")(
    test("roll should return correct result from boundary") {
      assert(roll(100))(equalTo(1)) &&
      assert(roll(99))(equalTo(100))
    },
    test("move should return correct result from boundary") {
      assert(move(10, 1))(equalTo(1)) &&
      assert(move(9, 2))(equalTo(1)) &&
      assert(move(9, 1))(equalTo(10))
    },
    test("turn should return correct state from test input") {
      assert(turn(Turn(4, 100, 0)))(equalTo(Turn(10, 3, 10))) &&
      assert(turn(Turn(8, 3, 0)))(equalTo(Turn(3, 6, 3))) &&
      assert(turn(Turn(10, 6, 10)))(equalTo(Turn(4, 9, 14))) &&
      assert(turn(Turn(3, 9, 3)))(equalTo(Turn(6, 12, 9))) &&
      assert(turn(Turn(4, 12, 14)))(equalTo(Turn(6, 15, 20))) &&
      assert(turn(Turn(6, 15, 9)))(equalTo(Turn(7, 18, 16))) &&
      assert(turn(Turn(6, 18, 20)))(equalTo(Turn(6, 21, 26))) &&
      assert(turn(Turn(7, 21, 16)))(equalTo(Turn(6, 24, 22)))
    },
    test("game should return correct score for test input") {
      assert(game(Seq(4, 8)))(equalTo(739785))
    },
    test("game should return correct score for real input") {
      for {
        positions <- ZFileReader.readLines("day-21-input-william.txt")(parse)
      } yield assert(game(positions))(equalTo(920079))
    }
  )
}
