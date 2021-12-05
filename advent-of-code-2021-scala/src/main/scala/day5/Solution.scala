package day5

object Solution {

  case class Point(x: Int, y: Int)
  case class Segment(p1: Point, p2: Point)

  def parseSegment(line: String): Segment = {
    val points = line.split(" -> ").map(p => Point(p.split(",")(0).toInt, p.split(",")(1).toInt))
    Segment(points(0), points(1))
  }

  def filterOnlyHorizontalAndVerticalSegments(input: List[Segment]): List[Segment] =
    input.filter(s => s.p1.x == s.p2.x || s.p1.y == s.p2.y)

  def getSumOfHorizontalAndVerticalIntersections(input: List[Segment]): Int = {
    val allPoints = input.map { segment =>
      for {
        x <- (segment.p1.x min segment.p2.x) to (segment.p1.x max segment.p2.x)
        y <- (segment.p1.y min segment.p2.y) to (segment.p1.y max segment.p2.y)
      } yield {
        Point(x, y)
      }
    }
    allPoints.flatten.groupBy(identity).map(_._2.size).filter(_ >= 2).size
  }

}
