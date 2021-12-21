package day21

object Solution {

  def parse(line: String): Int = line match {
    case s"Player $_ starting position: $p" => p.toInt
  }

  case class Turn(position: Int, lastDiceThrow: Int, score: Int)

  def turn(previous: Turn): Turn = {

    val firstRoll  = roll(previous.lastDiceThrow)
    val secondRoll = roll(firstRoll)
    val thirdRoll  = roll(secondRoll)

    val newPosition = move(previous.position, firstRoll + secondRoll + thirdRoll)
    val newScore    = previous.score + newPosition
    // println("turn", firstRoll, secondRoll, thirdRoll, newPosition)

    Turn(newPosition, thirdRoll, newScore)
  }

  def roll(prev: Int): Int = if (prev % 100 == 0) 1 else prev + 1

  def move(prev: Int, move: Int): Int = if ((prev + move) % 10 == 0) 10 else (prev + move) % 10

  def game(positions: Seq[Int]): Int = {
    val playerOnePosition = positions(0)
    val playerTwoPosition = positions(1)

    def helper(isPlayerOne: Boolean, prevPlayerOneTurn: Turn, prevPlayerTwoTurn: Turn, numDiceRolls: Int): Int = {
    //   println(if (isPlayerOne) "p1" else "p2", prevPlayerOneTurn, prevPlayerTwoTurn, numDiceRolls)
      if (prevPlayerOneTurn.score >= 1000 || prevPlayerTwoTurn.score >= 1000) {
        if (prevPlayerOneTurn.score < prevPlayerTwoTurn.score)
          prevPlayerOneTurn.score * numDiceRolls
        else
          prevPlayerTwoTurn.score * numDiceRolls
      } else if (isPlayerOne) {
        val newTurn = turn(prevPlayerOneTurn.copy(lastDiceThrow = prevPlayerTwoTurn.lastDiceThrow))
        helper(false, newTurn, prevPlayerTwoTurn, numDiceRolls + 3)
      } else {
        val newTurn = turn(prevPlayerTwoTurn.copy(lastDiceThrow = prevPlayerOneTurn.lastDiceThrow))
        helper(true, prevPlayerOneTurn, newTurn, numDiceRolls + 3)
      }
    }

    helper(false, turn(Turn(playerOnePosition, 100, 0)), Turn(playerTwoPosition, 3, 0), 3)
  }
}
