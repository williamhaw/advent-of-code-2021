package day3

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

}
