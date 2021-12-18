package day11

import zio.test._
import Assertion._
import common.ZFileReader
import day11.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day11.SolutionSpec")(
    test("step should return grid after multiple flashes") {
      val input = Seq(
        Seq(1, 1, 1, 1, 1),
        Seq(1, 9, 9, 9, 1),
        Seq(1, 9, 1, 9, 1),
        Seq(1, 9, 9, 9, 1),
        Seq(1, 1, 1, 1, 1)
      )
      assert(step(input))(
        equalTo(
          Seq(
            Seq(3, 4, 5, 4, 3),
            Seq(4, 0, 0, 0, 4),
            Seq(5, 0, 0, 0, 5),
            Seq(4, 0, 0, 0, 4),
            Seq(3, 4, 5, 4, 3)
          )
        )
      )
    },
    test("step should return grid after one step from test input") {
      for {
        input <- ZFileReader.readLines("day11-test-input.txt")(parse)
      } yield assert(step(input))(
        equalTo(
          Seq(
            Seq(6, 5, 9, 4, 2, 5, 4, 3, 3, 4),
            Seq(3, 8, 5, 6, 9, 6, 5, 8, 2, 2),
            Seq(6, 3, 7, 5, 6, 6, 7, 2, 8, 4),
            Seq(7, 2, 5, 2, 4, 4, 7, 2, 5, 7),
            Seq(7, 4, 6, 8, 4, 9, 6, 5, 8, 9),
            Seq(5, 2, 7, 8, 6, 3, 5, 7, 5, 6),
            Seq(3, 2, 8, 7, 9, 5, 2, 8, 3, 2),
            Seq(7, 9, 9, 3, 9, 9, 2, 2, 4, 5),
            Seq(5, 9, 5, 7, 9, 5, 9, 6, 6, 5),
            Seq(6, 3, 9, 4, 8, 6, 2, 6, 3, 7)
          )
        )
      )
    },
    test("stepN should return grid after n steps from test input") {
      for {
        input <- ZFileReader.readLines("day11-test-input.txt")(parse)
      } yield assert(stepN(2, input))(
        equalTo(
          Seq(
            Seq(8, 8, 0, 7, 4, 7, 6, 5, 5, 5),
            Seq(5, 0, 8, 9, 0, 8, 7, 0, 5, 4),
            Seq(8, 5, 9, 7, 8, 8, 9, 6, 0, 8),
            Seq(8, 4, 8, 5, 7, 6, 9, 6, 0, 0),
            Seq(8, 7, 0, 0, 9, 0, 8, 8, 0, 0),
            Seq(6, 6, 0, 0, 0, 8, 8, 9, 8, 9),
            Seq(6, 8, 0, 0, 0, 0, 5, 9, 4, 3),
            Seq(0, 0, 0, 0, 0, 0, 7, 4, 5, 6),
            Seq(9, 0, 0, 0, 0, 0, 0, 8, 7, 6),
            Seq(8, 7, 0, 0, 0, 0, 6, 8, 4, 8)
          )
        )
      ) && assert(stepN(10, input))(
        equalTo(
          Seq(
            Seq(0, 4, 8, 1, 1, 1, 2, 9, 7, 6),
            Seq(0, 0, 3, 1, 1, 1, 2, 0, 0, 9),
            Seq(0, 0, 4, 1, 1, 1, 2, 5, 0, 4),
            Seq(0, 0, 8, 1, 1, 1, 1, 4, 0, 6),
            Seq(0, 0, 9, 9, 1, 1, 1, 3, 0, 6),
            Seq(0, 0, 9, 3, 5, 1, 1, 2, 3, 3),
            Seq(0, 4, 4, 2, 3, 6, 1, 1, 3, 0),
            Seq(5, 5, 3, 2, 2, 5, 2, 3, 5, 0),
            Seq(0, 5, 3, 2, 2, 5, 0, 6, 0, 0),
            Seq(0, 0, 3, 2, 2, 4, 0, 0, 0, 0)
          )
        )
      )
    },
    test("stepNTotalFlashes should return total number of flashes after n steps from test input") {
      for {
        input <- ZFileReader.readLines("day11-test-input.txt")(parse)
      } yield assert(stepNTotalFlashes(10, input, 0))(equalTo(204)) &&
        assert(stepNTotalFlashes(100, input, 0))(equalTo(1656))
    },
    test("stepNTotalFlashes should return total number of flashes after n steps from real input") {
      for {
        input <- ZFileReader.readLines("day-11-input-william.txt")(parse)
      } yield assert(stepNTotalFlashes(100, input, 0))(equalTo(1591))
    },
    test("stepSynchronized should return step where all octopuses flash from test input") {
      for {
        input <- ZFileReader.readLines("day11-test-input.txt")(parse)
      } yield assert(stepSynchronized(input))(equalTo(195))
    },
    test("stepSynchronized should return step where all octopuses flash from real input") {
      for {
        input <- ZFileReader.readLines("day-11-input-william.txt")(parse)
      } yield assert(stepSynchronized(input))(equalTo(314))
    }
  )
}
