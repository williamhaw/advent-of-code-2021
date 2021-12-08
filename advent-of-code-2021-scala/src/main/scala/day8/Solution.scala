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

  def getDigitFromSignal(signals: Seq[String]): Map[Set[Char], Int] = {

    val `1` = signals.find(_.length() == 2).get.toSet
    val `4` = signals.find(_.length() == 4).get.toSet

    signals.map { signal =>
      signal match {
        case s if s.length() == 2 => (s.toSet, 1)
        case s if s.length() == 4 => (s.toSet, 4)
        case s if s.length() == 3 => (s.toSet, 7)
        case s if s.length() == 7 => (s.toSet, 8)
        case s if s.length() == 6 =>
          s.toSet match {
            case chars if chars.intersect(`1`).size == 1 => (chars, 6)
            case chars if chars.intersect(`4`).size == 4 => (chars, 9)
            case chars                                   => (chars, 0)
          }
        case s if s.length() == 5 =>
          s.toSet match {
            case chars if chars.intersect(`1`).size == 2 => (chars, 3)
            case chars if chars.intersect(`4`).size == 3 => (chars, 5)
            case chars                                   => (chars, 2)
          }
      }
    }.toMap
  }

  def getOutputFromSignal(outputDigits: Seq[String], signalMap: Map[Set[Char], Int]): Int =
    outputDigits.map(o => signalMap(o.toSet)).foldLeft(0)((acc, ele) => acc * 10 + ele)

  def getSumOfOutputs(signalOutputs: List[SignalOutput]): Int =
    signalOutputs.map(so => getOutputFromSignal(so.outputDigits, getDigitFromSignal(so.signals))).sum

}
