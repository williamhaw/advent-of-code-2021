package day4

import zio.test._
import Assertion._
import common.ZFileReader
import day4.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day4.SolutionSpec")(
    test("markEntries should mark board correctly") {
      val board = List(
        List(Entry(22, false), Entry(13, false), Entry(17, false), Entry(11, false), Entry(0, true)),
        List(Entry(8, false), Entry(2, false), Entry(23, false), Entry(4, false), Entry(24, false)),
        List(Entry(21, false), Entry(9, false), Entry(14, false), Entry(16, false), Entry(7, false)),
        List(Entry(6, false), Entry(10, false), Entry(3, false), Entry(18, false), Entry(5, false)),
        List(Entry(1, false), Entry(12, false), Entry(20, false), Entry(15, false), Entry(19, false))
      )

      assert(markEntries(9, board))(
        equalTo(
          List(
            List(Entry(22, false), Entry(13, false), Entry(17, false), Entry(11, false), Entry(0, true)),
            List(Entry(8, false), Entry(2, false), Entry(23, false), Entry(4, false), Entry(24, false)),
            List(Entry(21, false), Entry(9, true), Entry(14, false), Entry(16, false), Entry(7, false)),
            List(Entry(6, false), Entry(10, false), Entry(3, false), Entry(18, false), Entry(5, false)),
            List(Entry(1, false), Entry(12, false), Entry(20, false), Entry(15, false), Entry(19, false))
          )
        )
      )
    },
    test("isWon should check win status") {
      val markedNotWon = List(
        List(Entry(22, false), Entry(13, false), Entry(17, false), Entry(11, false), Entry(0, true)),
        List(Entry(8, false), Entry(2, false), Entry(23, false), Entry(4, false), Entry(24, false)),
        List(Entry(21, false), Entry(9, false), Entry(14, false), Entry(16, false), Entry(7, false)),
        List(Entry(6, false), Entry(10, false), Entry(3, false), Entry(18, false), Entry(5, false)),
        List(Entry(1, false), Entry(12, false), Entry(20, false), Entry(15, false), Entry(19, false))
      )

      val markedRowWon = List(
        List(Entry(22, true), Entry(13, true), Entry(17, true), Entry(11, true), Entry(0, true)),
        List(Entry(8, false), Entry(2, false), Entry(23, false), Entry(4, false), Entry(24, false)),
        List(Entry(21, false), Entry(9, false), Entry(14, false), Entry(16, false), Entry(7, false)),
        List(Entry(6, false), Entry(10, false), Entry(3, false), Entry(18, false), Entry(5, false)),
        List(Entry(1, false), Entry(12, false), Entry(20, false), Entry(15, false), Entry(19, false))
      )

      val markedColumnWon = List(
        List(Entry(22, false), Entry(13, false), Entry(17, false), Entry(11, false), Entry(0, true)),
        List(Entry(8, false), Entry(2, false), Entry(23, false), Entry(4, false), Entry(24, true)),
        List(Entry(21, false), Entry(9, false), Entry(14, false), Entry(16, false), Entry(7, true)),
        List(Entry(6, false), Entry(10, false), Entry(3, false), Entry(18, false), Entry(5, true)),
        List(Entry(1, false), Entry(12, false), Entry(20, false), Entry(15, false), Entry(19, true))
      )

      val markedRowAndColumnWon = List(
        List(Entry(22, true), Entry(13, true), Entry(17, true), Entry(11, true), Entry(0, true)),
        List(Entry(8, false), Entry(2, false), Entry(23, false), Entry(4, false), Entry(24, true)),
        List(Entry(21, false), Entry(9, false), Entry(14, false), Entry(16, false), Entry(7, true)),
        List(Entry(6, false), Entry(10, false), Entry(3, false), Entry(18, false), Entry(5, true)),
        List(Entry(1, false), Entry(12, false), Entry(20, false), Entry(15, false), Entry(19, true))
      )

      assert(isWon(markedNotWon))(equalTo(false)) &&
      assert(isWon(markedRowWon))(equalTo(true)) &&
      assert(isWon(markedColumnWon))(equalTo(true)) &&
      assert(isWon(markedRowAndColumnWon))(equalTo(true))
    },
    test("getScore should get test score") {
      val board = List(
        List(Entry(14, true), Entry(21, true), Entry(17, true), Entry(24, true), Entry(4, true)),
        List(Entry(10, false), Entry(16, false), Entry(15, false), Entry(9, true), Entry(19, false)),
        List(Entry(18, false), Entry(8, false), Entry(23, true), Entry(26, false), Entry(20, false)),
        List(Entry(22, false), Entry(11, true), Entry(13, false), Entry(6, false), Entry(5, true)),
        List(Entry(2, true), Entry(0, true), Entry(12, false), Entry(3, false), Entry(7, true))
      )
      assert(getScore(24, board))(equalTo(4512))
    },
    test("getScoreForFirstWinningBoard should return correct score for test inputs") {
      for {
        numbers    <- ZFileReader.readLines("day4-test-numbers.txt")(parseNumbers).head
        boardLines <- ZFileReader.readLines("day4-test-boards.txt")(parseBoardLinesToEntry)
        boards = boardLinesToBoards(boardLines)
      } yield assert(getScoreForFirstWinningBoard(boards, numbers))(equalTo(4512))
    },
    test("getScoreForFirstWinningBoard should return correct score for real inputs") {
      for {
        numbers    <- ZFileReader.readLines("day-4-input-numbers-william.txt")(parseNumbers).head
        boardLines <- ZFileReader.readLines("day-4-input-boards-william.txt")(parseBoardLinesToEntry)
        boards = boardLinesToBoards(boardLines)
      } yield assert(getScoreForFirstWinningBoard(boards, numbers))(equalTo(31424))
    },
    test("getScoreForLastWinningBoard should return correct score for test inputs") {
      for {
        numbers    <- ZFileReader.readLines("day4-test-numbers.txt")(parseNumbers).head
        boardLines <- ZFileReader.readLines("day4-test-boards.txt")(parseBoardLinesToEntry)
        boards = boardLinesToBoards(boardLines)
      } yield assert(getScoreForLastWinningBoard(boards, numbers))(equalTo(1924))
    },
    test("getScoreForLastWinningBoard should return correct score for real inputs") {
      for {
        numbers    <- ZFileReader.readLines("day-4-input-numbers-william.txt")(parseNumbers).head
        boardLines <- ZFileReader.readLines("day-4-input-boards-william.txt")(parseBoardLinesToEntry)
        boards = boardLinesToBoards(boardLines)
      } yield assert(getScoreForLastWinningBoard(boards, numbers))(equalTo(23042))
    }
  )
}
