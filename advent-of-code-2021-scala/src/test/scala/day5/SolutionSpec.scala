package day5

import zio.test._
import Assertion._
import common.ZFileReader
import day5.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day5.SolutionSpec")(
    test("getSumOfHorizontalAndVerticalIntersections should return correct sum of intersections from test input")(
      for {
        segments <- ZFileReader.readLines("day5-test-input.txt")(parseSegment)
      } yield assert(getSumOfHorizontalAndVerticalIntersections(segments))(equalTo(5))
    ),
    test("getSumOfHorizontalAndVerticalIntersections should return correct sum of intersections from real input")(
      for {
        segments <- ZFileReader.readLines("day-5-input-william.txt")(parseSegment)
      } yield assert(getSumOfHorizontalAndVerticalIntersections(segments))(equalTo(5608))
    ),
    test("getDiagonalPoints") {
      val segmentsPlusPlus   = List(Segment(Point(0, 0), Point(3, 3)))
      val segmentsMinusPlus  = List(Segment(Point(3, 0), Point(0, 3)))
      val segmentsPlusMinus  = List(Segment(Point(0, 3), Point(3, 0)))
      val segmentsMinusMinus = List(Segment(Point(3, 3), Point(0, 0)))
      assert(getDiagonalPoints(segmentsPlusPlus))(
        equalTo(List(Point(0, 0), Point(1, 1), Point(2, 2), Point(3, 3)))
      ) &&
      assert(getDiagonalPoints(segmentsMinusPlus))(
        equalTo(List(Point(0, 3), Point(1, 2), Point(2, 1), Point(3, 0)))
      ) &&
      assert(getDiagonalPoints(segmentsPlusMinus))(
        equalTo(List(Point(0, 3), Point(1, 2), Point(2, 1), Point(3, 0)))
      ) &&
      assert(getDiagonalPoints(segmentsMinusMinus))(
        equalTo(List(Point(0, 0), Point(1, 1), Point(2, 2), Point(3, 3)))
      )
    },
    test("getSumOfHorizontalVerticalDiagonalIntersections should return correct sum of intersections from test input")(
      for {
        segments <- ZFileReader.readLines("day5-test-input.txt")(parseSegment)
      } yield assert(getSumOfHorizontalVerticalDiagonalIntersections(segments))(equalTo(12))
    ),
    test("getSumOfHorizontalVerticalDiagonalIntersections should return correct sum of intersections from real input")(
      for {
        segments <- ZFileReader.readLines("day-5-input-william.txt")(parseSegment)
      } yield assert(getSumOfHorizontalVerticalDiagonalIntersections(segments))(equalTo(20299))
    )
  )
}
