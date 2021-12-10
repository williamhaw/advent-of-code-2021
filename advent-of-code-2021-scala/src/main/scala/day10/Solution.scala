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

  val openBrackets = Set('(', '[', '{', '<')
  val correspondingBrackets = Map(
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    '>' -> '<'
  )

  def getErrorCharacter(line: String): Option[Char] = {

    @tailrec
    def helper(remaining: String, stack: Seq[Char]): Option[Char] = {
      if (remaining.isEmpty)
        None
      else if (openBrackets.contains(remaining.head))
        helper(remaining.tail, stack.prepended(remaining.head))
      else if (correspondingBrackets(remaining.head) == stack.head)
        helper(remaining.tail, stack.tail)
      else
        Some(remaining.head)
    }
    val ret = helper(line, List())
    ret
  }

  def getErrorScore(input: Seq[String]): Int =
    input.map(getErrorCharacter).map(_.fold(0)(points)).sum

}
