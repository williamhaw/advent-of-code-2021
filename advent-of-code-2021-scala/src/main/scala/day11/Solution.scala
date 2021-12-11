package day11

object Solution {

  def parse(line: String): Seq[Int] = line.strip().map(_.toInt - 48)

}
