package day12

import zio.test._
import Assertion._
import common.ZFileReader
import day12.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day12.SolutionSpec")(
    test("getAdjacencyList should return the adjacency list for test input") {
      for {
        input <- ZFileReader.readLines("day12-test-input-small.txt")(parse)
      } yield assert(getAdjacencyList(input))(
        equalTo(
          Map(
            "A"     -> List("c", "b", "end"),
            "b"     -> List("d", "end", "A"),
            "end"   -> List("A", "b"),
            "c"     -> List("A"),
            "start" -> List("A", "b"),
            "d"     -> List("b")
          )
        )
      )
    },
    test("getPaths should return the correct number of paths for test input") {
      for {
        smallInput  <- ZFileReader.readLines("day12-test-input-small.txt")(parse)
        mediumInput <- ZFileReader.readLines("day12-test-input-medium.txt")(parse)
        largeInput  <- ZFileReader.readLines("day12-test-input-large.txt")(parse)
      } yield assert(getPaths(getAdjacencyList(smallInput)).size)(equalTo(10)) &&
        assert(getPaths(getAdjacencyList(mediumInput)).size)(equalTo(19)) &&
        assert(getPaths(getAdjacencyList(largeInput)).size)(equalTo(226))
    },
    test("getPaths should return the correct number of paths for real input") {
      for {
        input <- ZFileReader.readLines("day-12-input-william.txt")(parse)
      } yield assert(getPaths(getAdjacencyList(input)).size)(equalTo(3495))
    },
    test("getPathsSmallCaveTwice should return the correct number of paths for test input") {
      for {
        smallInput  <- ZFileReader.readLines("day12-test-input-small.txt")(parse)
        mediumInput <- ZFileReader.readLines("day12-test-input-medium.txt")(parse)
        largeInput  <- ZFileReader.readLines("day12-test-input-large.txt")(parse)
      } yield assert(getPathsSmallCaveTwice(getAdjacencyList(smallInput)).size)(equalTo(36)) &&
        assert(getPathsSmallCaveTwice(getAdjacencyList(mediumInput)).size)(equalTo(103)) &&
        assert(getPathsSmallCaveTwice(getAdjacencyList(largeInput)).size)(equalTo(3509))
    },
    test("getPathsSmallCaveTwice should return the correct number of paths for real input") {
      for {
        input <- ZFileReader.readLines("day-12-input-william.txt")(parse)
      } yield assert(getPathsSmallCaveTwice(getAdjacencyList(input)).size)(equalTo(94849))
    }
  )
}
