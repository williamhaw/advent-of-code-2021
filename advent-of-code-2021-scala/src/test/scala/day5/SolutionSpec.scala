package day5

import zio.test._
import Assertion._
import common.ZFileReader
import day5.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day5.SolutionSpec")(
    test("")(assert(true)(equalTo(true)))
  )
}
