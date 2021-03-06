package day5

object Solution {

  case class Point(x: Int, y: Int)
  case class Segment(p1: Point, p2: Point) {
    val left  = if (p1.x < p2.x) p1 else p2
    val right = if (p1.x > p2.x) p1 else p2
  }

  def parseSegment(line: String): Segment =
    line match {
      case s"$x1,$y1 -> $x2,$y2" => Segment(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }

  def filterOnlyHorizontalAndVerticalSegments(input: List[Segment]): List[Segment] =
    input.filter(s => s.p1.x == s.p2.x || s.p1.y == s.p2.y)

  def filterDiagonalSegments(input: List[Segment]): List[Segment] =
    input.filter(s => (s.p1.x - s.p2.x).abs == (s.p1.y - s.p2.y).abs)

  def getHorizontalAndVerticalPoints(input: List[Segment]): List[Point] = {
    val points = filterOnlyHorizontalAndVerticalSegments(input).map { segment =>
      for {
        x <- (segment.p1.x min segment.p2.x) to (segment.p1.x max segment.p2.x)
        y <- (segment.p1.y min segment.p2.y) to (segment.p1.y max segment.p2.y)
      } yield {
        Point(x, y)
      }
    }
    points.flatten
  }

  def getSumOfHorizontalAndVerticalIntersections(input: List[Segment]): Int = {
    val points = getHorizontalAndVerticalPoints(input)
    points.groupBy(identity).map(_._2.size).filter(_ >= 2).size
  }

  def getDiagonalPoints(input: List[Segment]): List[Point] = {
    val points = filterDiagonalSegments(input).map { segment =>
      val deltaY = (segment.left.y, segment.right.y) match {
        case (y1, y2) if y1 > y2 => -1
        case (y1, y2) if y1 < y2 => 1
        case _                   => 0
      }

      for {
        x <- segment.left.x to segment.right.x
      } yield {
        Point(x, segment.left.y + deltaY * (x - segment.left.x))
      }
    }
    points.flatten
  }

  def getSumOfHorizontalVerticalDiagonalIntersections(input: List[Segment]): Int = {
    val points = getHorizontalAndVerticalPoints(input) :++ getDiagonalPoints(input)
    points.groupBy(identity).map(_._2.size).filter(_ >= 2).size
  }

}
