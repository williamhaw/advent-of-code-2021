package day13

object Solution {

  case class Point(x: Int, y: Int)

  sealed trait Dimension
  case object X extends Dimension
  case object Y extends Dimension

  case class Fold(dimension: Dimension, value: Int)

  def parseCoords(line: String): Point = line match { case s"$x,$y" => Point(x.toInt, y.toInt) }

  def parseFolds(line: String): Fold = line match { case s"fold along $d=$v" => Fold(if (d == "x") X else Y, v.toInt) }

  def performFold(points: Seq[Point], fold: Fold): Seq[Point] = points.map { p =>
    fold match {
      case Fold(X, v) => if (p.x > v) Point(v - (p.x - v), p.y) else p
      case Fold(Y, v) => if (p.y > v) Point(p.x, v - (p.y - v)) else p
    }
  }.distinct

}
