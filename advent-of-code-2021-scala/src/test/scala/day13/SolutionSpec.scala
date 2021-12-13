package day13

import zio.test._
import Assertion._
import common.ZFileReader
import day13.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day13.SolutionSpec")(
    test("performFold should return points left after one fold on test input") {
      for {
        points <- ZFileReader.readLines("day13-test-input-coords.txt")(parseCoords)
        folds  <- ZFileReader.readLines("day13-test-input-folds.txt")(parseFolds)
      } yield assert(performFold(points, folds.head).size)(equalTo(17))
    },
    test("performFold should return points left after one fold on real input") {
      for {
        points <- ZFileReader.readLines("day-13-input-coords-william.txt")(parseCoords)
        folds  <- ZFileReader.readLines("day-13-input-folds-william.txt")(parseFolds)
      } yield assert(performFold(points, folds.head).size)(equalTo(602))
    }
  )
}
