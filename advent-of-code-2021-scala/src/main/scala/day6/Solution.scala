package day6

import scala.annotation.tailrec

object Solution {

  /** @param input
    * @return
    *   Map where key is the day and value is the number of fishes with timer values corresponding to that day.
    */
  def parse(input: String): Map[Int, Long] = {
    val empty = Map(0 -> 0L, 1 -> 0L, 2 -> 0L, 3 -> 0L, 4 -> 0L, 5 -> 0L, 6 -> 0L, 7 -> 0L, 8 -> 0L)
    val values = input
      .strip()
      .split(",")
      .map(_.toInt)
      .groupBy(identity)
      .map { case (timer, array) => (timer, array.size.toLong) }

    (empty.toSeq ++ values.toSeq).groupBy(_._1).mapValues(_.map(_._2).sum).toMap
  }

  def simulateDay(timers: Map[Int, Long]): Map[Int, Long] =
    timers.keys.flatMap { t =>
      t match {
        case 0                 => List((6, timers(0) + timers(7)), (8, timers(0)))
        case tNow if tNow == 7 => List()
        case tNow              => List((tNow - 1, timers(tNow)))
      }
    }.toMap

  @tailrec
  def runSimForNDays(days: Int, timers: Map[Int, Long]): Map[Int, Long] =
    if (days == 0)
      timers
    else
      runSimForNDays(days - 1, simulateDay(timers))

  def getTotalFishCount(timers: Map[Int, Long]): Long = timers.foldLeft(0L)(_ + _._2)
}
