package day2

object Solution {

  case class PositionDepth(horizontal: Int, depth: Int)
  case class PositionDepthAim(horizontal: Int, depth: Int, aim: Int)

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

    def findPositionWithAim(commands: List[Command]): PositionDepthAim =
        commands.foldLeft(PositionDepthAim(0,0,0))((current: PositionDepthAim, c: Command) => c match {
            case Forward(units) => current.copy(horizontal = current.horizontal + units, depth = current.depth + current.aim * units)
            case Down(units) => current.copy(aim = current.aim + units)
            case Up(units) => current.copy(aim = current.aim - units)
        })
}
