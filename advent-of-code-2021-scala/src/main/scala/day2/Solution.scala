package day2

object Solution {

  case class PositionDepth(horizontal: Int, depth: Int)

  sealed trait Command
  case class Forward(units: Int) extends Command
  case class Down(units: Int)    extends Command
  case class Up(units: Int)      extends Command

  def parseCommand(line: String): Command = {
    line.split(" ") match {
      case Array("forward", u) => Forward(u.toInt)
      case Array("up", u)      => Up(u.toInt)
      case Array("down", u)    => Down(u.toInt)
    }
  }

  def findPosition(commands: List[Command]): PositionDepth =
    commands.foldLeft(PositionDepth(0, 0))((currentPostion: PositionDepth, c: Command) =>
      c match {
        case Forward(units) => currentPostion.copy(horizontal = currentPostion.horizontal + units)
        case Down(units)    => currentPostion.copy(depth = currentPostion.depth + units)
        case Up(units)      => currentPostion.copy(depth = currentPostion.depth - units)
      }
    )

}
