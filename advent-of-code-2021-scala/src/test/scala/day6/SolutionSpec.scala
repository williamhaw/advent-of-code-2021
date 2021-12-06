package day6

import zio.test._
import Assertion._
import common.ZFileReader
import day6.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day6.SolutionSpec")(
    test("simulateDay should return next day's timers for test input") {
      for {
        timers <- ZFileReader.readLines("day6-test-input.txt")(parse).head
      } yield assert(simulateDay(timers))(
        equalTo(Map(0 -> 1L, 1 -> 1L, 2 -> 2L, 3 -> 1L, 4 -> 0L, 5 -> 0L, 6 -> 0L, 7 -> 0L, 8 -> 0L))
      )
    },
    test("runSimForNDays should return the correct number of fish on the 18th day for test input") {
      for {
        timers <- ZFileReader.readLines("day6-test-input.txt")(parse).head
      } yield assert(getTotalFishCount(runSimForNDays(18, timers)))(equalTo(26L))
    },
    test("runSimForNDays should return the correct number of fish on the 256th day for test input") {
      for {
        timers <- ZFileReader.readLines("day6-test-input.txt")(parse).head
      } yield assert(getTotalFishCount(runSimForNDays(256, timers)))(equalTo(26984457539L))
    },
    test("runSimForNDays should return the correct number of fish on the 80th day for real input") {
      for {
        timers <- ZFileReader.readLines("day-6-input-william.txt")(parse).head
      } yield assert(getTotalFishCount(runSimForNDays(80, timers)))(equalTo(393019L))
    },
    test("runSimForNDays should return the correct number of fish on the 256th day for real input") {
      for {
        timers <- ZFileReader.readLines("day-6-input-william.txt")(parse).head
      } yield assert(getTotalFishCount(runSimForNDays(256, timers)))(equalTo(1757714216975L))
    }
  )
}
