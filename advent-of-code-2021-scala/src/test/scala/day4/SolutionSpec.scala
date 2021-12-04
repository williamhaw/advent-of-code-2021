package day4

import zio.test._
import Assertion._
import common.ZFileReader
import day4.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day4.SolutionSpec")(
    test("markBoard should mark board correctly") {
      val board = List(
        List(22, 13, 17, 11, 0),
        List(8, 2, 23, 4, 24),
        List(21, 9, 14, 16, 7),
        List(6, 10, 3, 18, 5),
        List(1, 12, 20, 15, 19)
      )

      val marked = List(
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0)
      )

      assert(markBoard(9, board, marked))(
        equalTo(
          List(
            List(0, 0, 0, 0, 1),
            List(0, 0, 0, 0, 0),
            List(0, 1, 0, 0, 0),
            List(0, 0, 0, 0, 0),
            List(0, 0, 0, 0, 0)
          )
        )
      )
    },
    test("isWon should check win status") {
      val markedNotWon = List(
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0)
      )

      val markedRowWon = List(
        List(1, 1, 1, 1, 1),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0),
        List(0, 0, 0, 0, 0)
      )

      val markedColumnWon = List(
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1)
      )

      val markedRowAndColumnWon = List(
        List(1, 1, 1, 1, 1),
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1),
        List(0, 0, 0, 0, 1)
      )

      assert(isWon(markedNotWon))(equalTo(false)) &&
      assert(isWon(markedRowWon))(equalTo(true)) &&
      assert(isWon(markedColumnWon))(equalTo(true)) &&
      assert(isWon(markedRowAndColumnWon))(equalTo(true))
    },
    test("getScore should get test score") {
      val board = List(
        List(14, 21, 17, 24, 4),
        List(10, 16, 15, 9, 19),
        List(18, 8, 23, 26, 20),
        List(22, 11, 13, 6, 5),
        List(2, 0, 12, 3, 7)
      )

      val marked = List(
        List(1, 1, 1, 1, 1),
        List(0, 0, 0, 1, 0),
        List(0, 0, 1, 0, 0),
        List(0, 1, 0, 0, 1),
        List(1, 1, 0, 0, 1)
      )
      assert(getScore(24, board, marked))(equalTo(4512))
    },
    test("playBingo should return correct score for test inputs") {
      for {
        numbers    <- ZFileReader.readLines("day4-test-numbers.txt")(parseNumbers).head
        boardLines <- ZFileReader.readLines("day4-test-boards.txt")(parseBoardLines)
        boards = boardLinesToBoards(boardLines)
      } yield assert(playBingo(boards, numbers))(equalTo(4512))
    },
    test("playBingo should return correct score for real inputs") {
      for {
        numbers    <- ZFileReader.readLines("day-4-input-numbers-william.txt")(parseNumbers).head
        boardLines <- ZFileReader.readLines("day-4-input-boards-william.txt")(parseBoardLines)
        boards = boardLinesToBoards(boardLines)
      } yield assert(playBingo(boards, numbers))(equalTo(31424))
    }
  )
}
