package day17

import scala.annotation.tailrec

object Solution {

  case class Position(x: Int, y: Int)
  case class Velocity(x: Int, y: Int)

  case class Bounds(b1: Int, b2: Int) {
    val min = Seq(b1, b2).min
    val max = Seq(b1, b2).max
  }

  case class Target(xBounds: Bounds, yBounds: Bounds) {
    val left   = xBounds.min
    val right  = xBounds.max
    val top    = yBounds.max
    val bottom = yBounds.min

    def isInside(p: Position): Boolean = p.x >= left && p.x <= right && p.y >= bottom && p.y <= top
  }

  def parse(line: String): Target = line match {
    case s"target area: x=$x1..$x2, y=$y1..$y2" => Target(Bounds(x1.toInt, x2.toInt), Bounds(y1.toInt, y2.toInt))
  }

  def step(initialPosition: Position, initialVelocity: Velocity): (Position, Velocity) =
    (
      initialPosition.copy(initialPosition.x + initialVelocity.x, initialPosition.y + initialVelocity.y),
      Velocity(
        if (initialVelocity.x > 0) initialVelocity.x - 1 else if (initialVelocity.x < 0) initialVelocity.x + 1 else 0,
        initialVelocity.y - 1
      )
    )

  type FinishWithinTarget = Boolean

  def simulateHighestY(
      initialPosition: Position,
      initialVelocity: Velocity,
      target: Target
  ): (Position, FinishWithinTarget) = {

    @tailrec
    def helper(highest: Position, position: Position, velocity: Velocity): (Position, Position) =
      if (target.isInside(position) || position.y < target.bottom)
        (highest, position)
      else {
        val (p, v) = step(position, velocity)
        helper(if (p.y > highest.y) p else highest, p, v)
      }

    val (highest, last) = helper(initialPosition, initialPosition, initialVelocity)
    (highest, target.isInside(last))
  }

  def searchHighestY(target: Target): Int = {
    val candidates = for {
      vX <- 0 to target.right
      vY <-
        (-target.top min target.top min target.bottom min -target.bottom) to (-target.top max target.top max target.bottom max -target.bottom)
    } yield simulateHighestY(Position(0, 0), Velocity(vX, vY), target)
    val (solution, _) = candidates.filter(_._2 == true).maxBy(_._1.y)
    solution.y
  }

  def getCountOfVelocitiesHit(target: Target): Int = {
    val candidates = for {
      vX <- 0 to target.right
      vY <-
        (-target.top min target.top min target.bottom min -target.bottom) to (-target.top max target.top max target.bottom max -target.bottom)
    } yield simulateHighestY(Position(0, 0), Velocity(vX, vY), target)
    candidates.filter(_._2 == true).size
  }

}
