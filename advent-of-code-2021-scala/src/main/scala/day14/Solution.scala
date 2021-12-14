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

}
