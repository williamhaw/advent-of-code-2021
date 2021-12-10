package day10

import scala.annotation.tailrec

object Solution {

  val parse: String => String = (line: String) => line

  val points = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val openBrackets               = Set('(', '[', '{', '<')
  val correspondingOpenBrackets  = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
  val correspondingCloseBrackets = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  def getErrorCharacter(line: String): Option[Char] =
    findError(line, List())._1

  @tailrec
  def findError(remaining: String, stack: Seq[Char]): (Option[Char], Seq[Char]) = {
    if (remaining.isEmpty)
      (None, stack)
    else if (openBrackets.contains(remaining.head))
      findError(remaining.tail, stack.prepended(remaining.head))
    else if (correspondingOpenBrackets(remaining.head) == stack.head)
      findError(remaining.tail, stack.tail)
    else
      (Some(remaining.head), stack)
  }

  def getErrorScore(input: Seq[String]): Int =
    input.map(getErrorCharacter).map(_.fold(0)(points)).sum

}
