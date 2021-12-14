package day14

import scala.annotation.tailrec

object Solution {

  def parseRules(line: String): (String, String) = line match {
    case s"$pair -> $insertChar" => pair -> insertChar
  }

  def performStep(polymer: String, rules: Map[String, String]): String =
    polymer
      .sliding(2)
      .map { currentPair =>
        rules.get(currentPair) match {
          case Some(value) => s"${currentPair(0)}$value${currentPair(1)}"
          case None        => currentPair
        }
      }
      .foldLeft("") { (accum, current) => if (accum.isEmpty()) current else accum + current.tail }

  def performNSteps(n: Int, polymer: String, rules: Map[String, String]): String = {
    @tailrec
    def helper(n: Int, input: String): String =
      if (n == 1)
        performStep(input, rules)
      else
        helper(n - 1, performStep(input, rules))
    helper(n, polymer)
  }

  def getScore(polymer: String): Int = {
    val counts = polymer.groupBy(identity).view.mapValues(_.size).values
    counts.max - counts.min
  }

  def processInput(polymer: String): Map[String, Long] =
    polymer.sliding(2).toSeq.groupBy(identity).view.mapValues(_.size.toLong).toMap

  def performStepOptimized(state: Map[String, Long], rules: Map[String, String]): Map[String, Long] =
    state.toSeq
      .flatMap { case (pair, count) =>
        rules.get(pair) match {
          case Some(value) => Seq(s"${pair(0)}$value" -> count, s"$value${pair(1)}" -> count)
          case None        => Seq(pair -> count)
        }
      }
      .groupMapReduce(_._1)(_._2)(_ + _)

  def performNStepsOptimized(n: Int, state: Map[String, Long], rules: Map[String, String]): Map[String, Long] = {
    @tailrec
    def helper(n: Int, input: Map[String, Long]): Map[String, Long] =
      if (n == 1)
        performStepOptimized(input, rules)
      else
        helper(n - 1, performStepOptimized(input, rules))
    helper(n, state)
  }

  def getScoreOptimized(state: Map[String, Long], template: String) = {
    val charCounts = (template.head -> 1L :: template.last -> 1L :: state.toList.flatMap { case (pair, count) =>
      Seq(pair(0) -> count, pair(1) -> count)
    }).groupMapReduce(_._1)(_._2)(_ + _).values.map(_ / 2)
    charCounts.max - charCounts.min
  }

}
