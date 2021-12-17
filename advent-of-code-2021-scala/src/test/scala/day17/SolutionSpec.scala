package day17

import zio.test._
import Assertion._
import common.ZFileReader
import day17.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day17.SolutionSpec")(
    test("simulateHighestY should return highest y for test input") {
      val target = "target area: x=20..30, y=-10..-5"
      assert(simulateHighestY(Position(0, 0), Velocity(7, 2), parse(target)))(
        equalTo((Position(13, 3), true))
      ) &&
      assert(simulateHighestY(Position(0, 0), Velocity(6, 3), parse(target)))(
        equalTo((Position(15, 6), true))
      ) &&
      assert(simulateHighestY(Position(0, 0), Velocity(9, 0), parse(target)))(
        equalTo((Position(0, 0), true))
      ) &&
      assert(simulateHighestY(Position(0, 0), Velocity(17, -4), parse(target)))(
        equalTo((Position(0, 0), false))
      ) &&
      assert(simulateHighestY(Position(0, 0), Velocity(6, 9), parse(target)))(
        equalTo((Position(21, 45), true))
      )
    },
    test("searchHighestY should return highest y for test input") {
      val target = "target area: x=20..30, y=-10..-5"
      assert(searchHighestY(parse(target)))(equalTo(45))
    },
    test("searchHighestY should return highest y for real input") {
      for {
        target <- ZFileReader.readLines("day-17-input-william.txt")(parse).head
      } yield assert(searchHighestY(target))(equalTo(4950))
    },
    test("getCountOfVelocitiesHit should return count of hits for test input") {
      val target = "target area: x=20..30, y=-10..-5"
      assert(getCountOfVelocitiesHit(parse(target)))(equalTo(112))
    },
    test("getCountOfVelocitiesHit should return count of hits for real input") {
      for {
        target <- ZFileReader.readLines("day-17-input-william.txt")(parse).head
      } yield assert(getCountOfVelocitiesHit(target))(equalTo(1477))
    }
  )
}
