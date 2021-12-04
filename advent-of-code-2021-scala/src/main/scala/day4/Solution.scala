package day4

import scala.annotation.tailrec

object Solution {

  val unmarkedBoard: List[List[Int]] = List(
    List(0, 0, 0, 0, 0),
    List(0, 0, 0, 0, 0),
    List(0, 0, 0, 0, 0),
    List(0, 0, 0, 0, 0),
    List(0, 0, 0, 0, 0)
  )

  def parseNumbers(line: String): List[Int] = line.split(",").map(_.toInt).toList

  def parseBoardLines(line: String): List[Int] = {
    line match {
      case "" => List.empty
      case _  => line.strip().split("\\s+").map(_.toInt).toList
    }
  }

  def boardLinesToBoards(input: List[List[Int]]): Seq[List[List[Int]]] = input.filter(!_.isEmpty).grouped(5).toList

  def markBoard(drawnNumber: Int, board: List[List[Int]], prevMarked: List[List[Int]]): List[List[Int]] =
    board.zipWithIndex.map { case (row, r) =>
      row.zipWithIndex.map { case (element, c) =>
        if (element == drawnNumber)
          1
        else
          prevMarked(r)(c)
      }
    }

  def isWon(marked: List[List[Int]]): Boolean = {
    val rowWon = (for {
      row <- marked
    } yield if (row.sum == 5) true else false).exists(identity)

    val colWon = (for {
      row <- marked.transpose
    } yield if (row.sum == 5) true else false).exists(identity)

    rowWon || colWon
  }

  def getScore(drawnNumber: Int, board: List[List[Int]], marked: List[List[Int]]): Int =
    if (isWon(marked)) {
      val unmarkedSum = board.zipWithIndex.map { case (row, r) =>
        row.zipWithIndex.map { case (element, c) =>
          if (marked(r)(c) == 0) element else 0
        }.sum
      }.sum

      unmarkedSum * drawnNumber

    } else
      throw new IllegalArgumentException("Board is not in winning state")

  def playBingo(boards: Seq[List[List[Int]]], numbers: List[Int]): Int = {

    @tailrec
    def helper(remainingNumbers: List[Int], marked: Seq[List[List[Int]]]): Int =
      if (remainingNumbers.isEmpty)
        0
      else {
        val currentDrawn = remainingNumbers.head
        val newMarked = for ((b, boardIndex) <- boards.zipWithIndex) yield {
          val m = marked(boardIndex)
          markBoard(currentDrawn, b, m)
        }

        if (newMarked.exists(isWon)) {
          newMarked.zipWithIndex.collect {
            case (m, mIndex) if isWon(m) => getScore(currentDrawn, boards(mIndex), m)
          }.head
        } else
          helper(remainingNumbers.tail, newMarked)
      }

    val startingBoards = for { _ <- 1 to boards.size } yield unmarkedBoard

    helper(numbers, startingBoards)
  }

  def playBingoWinLast(boards: Seq[List[List[Int]]], numbers: List[Int]): Int = {

    @tailrec
    def helper(
        boards: Seq[List[List[Int]]],
        remainingNumbers: List[Int],
        marked: Seq[List[List[Int]]]
    ): (Int, List[List[Int]], List[List[Int]]) = {
      val currentDrawn = remainingNumbers.head
      val newMarked = for ((b, boardIndex) <- boards.zipWithIndex) yield {
        val m = marked(boardIndex)
        markBoard(currentDrawn, b, m)
      }
      if (newMarked.exists(isWon)) {
        val newBoards = boards.filterNot(isWon)
        if (newBoards.size == 1) {
          (currentDrawn, newBoards.head, newMarked.head)
        } else {
          val wonIndexes      = newMarked.zipWithIndex.filter { case (m, _) => isWon(m) }.map(_._2).toSet
          val remainingBoards = boards.zipWithIndex.filter { case (_, i) => !wonIndexes.contains(i) }.map(_._1)
          val remainingMarked = newMarked.zipWithIndex.filter { case (_, i) => !wonIndexes.contains(i) }.map(_._1)
          helper(remainingBoards, remainingNumbers.tail, remainingMarked)
        }
      } else
        helper(boards, remainingNumbers.tail, newMarked)
    }

    val startingBoards                      = for { _ <- 1 to boards.size } yield unmarkedBoard
    val (lastNumber, lastBoard, lastMarked) = helper(boards, numbers, startingBoards)
    getScore(lastNumber, lastBoard, lastMarked)
  }

}
