package day5

import zio.test._
import Assertion._
import common.ZFileReader
import day5.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day5.SolutionSpec")(
    test("getSumOfHorizontalAndVerticalIntersections should return correct sum of intersections from test input")(
      for{
          segments <- ZFileReader.readLines("day5-test-input.txt")(parseSegment)
          filteredSegments = filterOnlyHorizontalAndVerticalSegments(segments)
      } yield assert(getSumOfHorizontalAndVerticalIntersections(filteredSegments))(equalTo(5))
    ),
    test("getSumOfHorizontalAndVerticalIntersections should return correct sum of intersections from real input")(
      for{
          segments <- ZFileReader.readLines("day-5-input-william.txt")(parseSegment)
          filteredSegments = filterOnlyHorizontalAndVerticalSegments(segments)
      } yield assert(getSumOfHorizontalAndVerticalIntersections(filteredSegments))(equalTo(5608))
    )
  )
}
