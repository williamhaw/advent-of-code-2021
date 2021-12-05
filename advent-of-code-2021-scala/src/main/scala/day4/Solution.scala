package day4

import scala.annotation.tailrec

object Solution {

  case class Entry(value: Int, isDrawn: Boolean)

  def parseNumbers(line: String): List[Int] = line.split(",").map(_.toInt).toList

  def parseBoardLinesToEntry(line: String): List[Entry] = line match {
    case "" => List.empty
    case _  => line.strip().split("\\s+").map(i => Entry(i.toInt, false)).toList
  }

  def boardLinesToBoards[T](input: List[List[T]]): Seq[List[List[T]]] = input.filter(!_.isEmpty).grouped(5).toList

  def markEntries(drawnNumber: Int, board: List[List[Entry]]) =
    board.map { row =>
      row.map { entry =>
        if (entry.value == drawnNumber)
          entry.copy(isDrawn = true)
        else
          entry
      }
    }

  def isWon(board: List[List[Entry]]): Boolean = {
    val rowWon = (for { row <- board } yield row.map(_.isDrawn).forall(identity)).exists(identity)
    val colWon = (for { col <- board.transpose } yield col.map(_.isDrawn).forall(identity)).exists(identity)
    rowWon || colWon
  }

  def getScore(drawnNumber: Int, board: List[List[Entry]]): Int =
    if (isWon(board))
      board.map(row => row.map(entry => if (!entry.isDrawn) entry.value else 0).sum).sum * drawnNumber
    else
      throw new IllegalArgumentException("Board is not in won state")

  def getScoreForFirstWinningBoard(boards: Seq[List[List[Entry]]], numbers: List[Int]): Int = {
    @tailrec
    def helper(remainingNumbers: List[Int], boards: Seq[List[List[Entry]]]): Int =
      if (remainingNumbers.isEmpty)
        0
      else {
        val currentDrawn = remainingNumbers.head
        val newBoards    = for (b <- boards) yield markEntries(currentDrawn, b)

        if (newBoards.exists(isWon))
          newBoards.collect { case b if (isWon(b)) => getScore(currentDrawn, b) }.head
        else
          helper(remainingNumbers.tail, newBoards)
      }

    helper(numbers, boards)
  }

  def getScoreForLastWinningBoard(boards: Seq[List[List[Entry]]], numbers: List[Int]): Int = {

    @tailrec
    def helper(remainingNumbers: List[Int], remainingBoards: Seq[List[List[Entry]]]): (Int, List[List[Entry]]) = {
      val currentDrawn = remainingNumbers.head
      val newBoards    = for (b <- remainingBoards) yield markEntries(currentDrawn, b)

      if (newBoards.exists(isWon)) {
        val notWonBoards = newBoards.filterNot(isWon)
        if (notWonBoards.isEmpty)
          (currentDrawn, newBoards.head)
        else {
          helper(remainingNumbers.tail, notWonBoards)
        }
      } else
        helper(remainingNumbers.tail, newBoards)
    }

    val (lastNumber, lastBoard) = helper(numbers, boards)
    getScore(lastNumber, lastBoard)
  }
}
