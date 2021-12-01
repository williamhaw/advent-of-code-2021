package day1

import zio.test._
import Assertion._
import common.ZFileReader

object SolutionSpec extends DefaultRunnableSpec{
  def spec = suite("day1.SolutionSpec")(
      test("countIncrease should get correct count"){
          for{
              lines <- ZFileReader.readLines("day-1-input-william.txt")(_.toInt)
          } yield assert(Solution.countIncrease(lines))(equalTo(1195))
      },
      test("countIncreaseWithTuple should get correct count"){
          for{
              lines <- ZFileReader.readLines("day-1-input-william.txt")(_.toInt)
          } yield assert(Solution.countIncreaseWithTuple(lines))(equalTo(1195))
      },
      test("countIncreaseWithWindow should get correct count"){
          for{
              lines <- ZFileReader.readLines("day-1-input-william.txt")(_.toInt)
          } yield assert(Solution.countIncreaseWithWindow(lines, 3))(equalTo(1235))
      }
  )
}
