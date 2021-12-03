package day3

import scala.annotation.tailrec

object Solution {

  def parse(line: String): List[Char] = line.toCharArray().toList

  def getGammaRate(input: List[List[Char]]): Int = {
    val columns = input.transpose

    val resultList = columns.map(col => if (col.count(_ == '0') > col.count(_ == '1')) '0' else '1')

    Integer.parseInt(resultList.mkString(""), 2)
  }

  def getEpsilonRate(input: List[List[Char]]): Int = {
    val columns = input.transpose

    val resultList = columns.map(col => if (col.count(_ == '0') < col.count(_ == '1')) '0' else '1')

    Integer.parseInt(resultList.mkString(""), 2)
  }

  def getOxygenGeneratorRating(input: List[List[Char]]): Int = {

    val disc = (c: List[Char]) => if (c.count(_ == '0') > c.count(_ == '1')) '0' else '1'

    Integer.parseInt(helper(input, 0, disc).mkString(""), 2)
  }

  def getCO2ScrubberRating(input: List[List[Char]]): Int = {
    val disc = (c: List[Char]) => if (c.count(_ == '0') <= c.count(_ == '1')) '0' else '1'

    Integer.parseInt(helper(input, 0, disc).mkString(""), 2)
  }

  @tailrec
  def helper(
      input: List[List[Char]],
      index: Int,
      discriminator: (List[Char]) => Char
  ): List[Char] = {
    if (input.size == 1)
      input.head
    else {
      val columns = input.transpose

      val currentColumn = columns(index)

      val currentBit = discriminator(currentColumn)

      val newInput: List[List[Char]] = input.filter(l => l(index) == currentBit)
      helper(newInput, index + 1, discriminator)
    }
  }
}
