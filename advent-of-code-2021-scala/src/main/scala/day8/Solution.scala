package day8

object Solution {

  case class SignalOutput(signals: Seq[String], outputDigits: Seq[String])

  def parse(line: String): SignalOutput =
    line match {
      case s"$signals | $outputs" => SignalOutput(signals.split(" "), outputs.split(" "))
    }

  val segmentCount = Set(
    2, // digit '1'
    4, // digit '4'
    3, // digit '7'
    7  // digit '8'
  )

  def getCountOfOneFourSevenEights(input: List[SignalOutput]): Int =
    input.foldLeft(0) { case (count, so) => count + so.outputDigits.count(s => segmentCount.contains(s.length())) }

}
