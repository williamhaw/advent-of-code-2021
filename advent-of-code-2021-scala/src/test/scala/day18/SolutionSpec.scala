package day18

import zio.test._
import Assertion._
import common.ZFileReader
import day18.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day18.SolutionSpec")(
    test("parse can parse snailfish numbers on test input") {
      assert(parse("[1,2]"))(equalTo(Pair(Number(1), Number(2)))) &&
      assert(parse("[[1,2],3]"))(equalTo(Pair(Pair(Number(1), Number(2)), Number(3)))) &&
      assert(parse("[9,[8,7]]"))(equalTo(Pair(Number(9), Pair(Number(8), Number(7))))) &&
      assert(parse("[[1,9],[8,5]]"))(equalTo(Pair(Pair(Number(1), Number(9)), Pair(Number(8), Number(5))))) &&
      assert(parse("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"))(
        equalTo(
          Pair(
            Pair(
              Pair(Pair(Number(1), Number(2)), Pair(Number(3), Number(4))),
              Pair(Pair(Number(5), Number(6)), Pair(Number(7), Number(8)))
            ),
            Number(9)
          )
        )
      ) &&
      assert(parse("[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"))(
        equalTo(
          Pair(
            Pair(
              Pair(Number(9), Pair(Number(3), Number(8))),
              Pair(Pair(Number(0), Number(9)), Number(6))
            ),
            Pair(
              Pair(
                Pair(Number(3), Number(7)),
                Pair(Number(4), Number(9))
              ),
              Number(3)
            )
          )
        )
      ) &&
      assert(parse("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"))(
        equalTo(
          Pair(
            Pair(
              Pair(
                Pair(Number(1), Number(3)),
                Pair(Number(5), Number(3))
              ),
              Pair(
                Pair(Number(1), Number(3)),
                Pair(Number(8), Number(7))
              )
            ),
            Pair(
              Pair(
                Pair(Number(4), Number(9)),
                Pair(Number(6), Number(9))
              ),
              Pair(
                Pair(Number(8), Number(2)),
                Pair(Number(7), Number(3))
              )
            )
          )
        )
      )
    },
    test("add returns added result for test input") {
      assert(parse("[1,2]") + parse("[[3,4],5]"))(
        equalTo(Pair(Pair(Number(1), Number(2)), Pair(Pair(Number(3), Number(4)), Number(5))))
      )
    },
    test("split returns split result correctly for test input") {
      assert(Number(10).split)(equalTo(Some(Pair(Number(5), Number(5))))) &&
      assert(Number(11).split)(equalTo(Some(Pair(Number(5), Number(6))))) &&
      assert(Number(12).split)(equalTo(Some(Pair(Number(6), Number(6)))))
    },
    test("explode returns exploded result correctly for test input") {
      assert(Number(1).explode)(equalTo(None)) &&
      assert(parse("[[[[[9,8],1],2],3],4]").explode)(
        equalTo(
          Some(
            Pair(
              Pair(
                Pair(
                  Pair(Number(0), Number(9)),
                  Number(2)
                ),
                Number(3)
              ),
              Number(4)
            )
          )
        )
      ) &&
      assert(parse("[7,[6,[5,[4,[3,2]]]]]").explode)(
        equalTo(
          Some(
            Pair(
              Number(7),
              Pair(
                Number(6),
                Pair(
                  Number(5),
                  Pair(Number(7), Number(0))
                )
              )
            )
          )
        )
      ) &&
      assert(parse("[[6,[5,[4,[3,2]]]],1]").explode)(
        equalTo(
          Some(
            Pair(
              Pair(
                Number(6),
                Pair(
                  Number(5),
                  Pair(Number(7), Number(0))
                )
              ),
              Number(3)
            )
          )
        )
      ) &&
      assert(parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode)(
        equalTo(
          Some(
            Pair(
              Pair(
                Number(3),
                Pair(
                  Number(2),
                  Pair(Number(8), Number(0))
                )
              ),
              Pair(
                Number(9),
                Pair(
                  Number(5),
                  Pair(
                    Number(4),
                    Pair(Number(3), Number(2))
                  )
                )
              )
            )
          )
        )
      ) &&
      assert(parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode)(
        equalTo(
          Some(
            Pair(
              Pair(
                Number(3),
                Pair(
                  Number(2),
                  Pair(Number(8), Number(0))
                )
              ),
              Pair(
                Number(9),
                Pair(
                  Number(5),
                  Pair(Number(7), Number(0))
                )
              )
            )
          )
        )
      )
    },
    test("reduce should return reduced result from test input") {
      assert((parse("[[[[4,3],4],4],[7,[[8,4],9]]]") + parse("[1,1]")).reduce)(
        equalTo(
          Pair(
            Pair(
              Pair(
                Pair(Number(0), Number(7)),
                Number(4)
              ),
              Pair(
                Pair(Number(7), Number(8)),
                Pair(Number(6), Number(0))
              )
            ),
            Pair(Number(8), Number(1))
          )
        )
      )
    },
    test("sum should return summed result from test input") {
      assert(sum(Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]").map(parse)))(
        equalTo(
          Pair(
            Pair(
              Pair(
                Pair(Number(1), Number(1)),
                Pair(Number(2), Number(2))
              ),
              Pair(Number(3), Number(3))
            ),
            Pair(Number(4), Number(4))
          )
        )
      ) &&
      assert(sum(Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]").map(parse)))(
        equalTo(
          Pair(
            Pair(
              Pair(
                Pair(Number(3), Number(0)),
                Pair(Number(5), Number(3))
              ),
              Pair(Number(4), Number(4))
            ),
            Pair(Number(5), Number(5))
          )
        )
      ) &&
      assert(sum(Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]").map(parse)))(
        equalTo(
          Pair(
            Pair(
              Pair(
                Pair(Number(5), Number(0)),
                Pair(Number(7), Number(4))
              ),
              Pair(Number(5), Number(5))
            ),
            Pair(Number(6), Number(6))
          )
        )
      )
    },
    test("sum should return summed result from larger test input") {
      for {
        input1 <- ZFileReader.readLines("day18-test-input-1.txt")(parse)
        input2 <- ZFileReader.readLines("day18-test-input-2.txt")(parse)
      } yield assert(sum(input1))(
        equalTo(
          Pair(
            Pair(
              Pair(
                Pair(Number(8), Number(7)),
                Pair(Number(7), Number(7))
              ),
              Pair(
                Pair(Number(8), Number(6)),
                Pair(Number(7), Number(7))
              )
            ),
            Pair(
              Pair(
                Pair(Number(0), Number(7)),
                Pair(Number(6), Number(6))
              ),
              Pair(Number(8), Number(7))
            )
          )
        )
      ) &&
        assert(sum(input2))(
          equalTo(
            Pair(
              Pair(
                Pair(
                  Pair(Number(6), Number(6)),
                  Pair(Number(7), Number(6))
                ),
                Pair(
                  Pair(Number(7), Number(7)),
                  Pair(Number(7), Number(0))
                )
              ),
              Pair(
                Pair(
                  Pair(Number(7), Number(7)),
                  Pair(Number(7), Number(7))
                ),
                Pair(
                  Pair(Number(7), Number(8)),
                  Pair(Number(9), Number(9))
                )
              )
            )
          )
        )
    },
    test("magnitude should return correct magnitude for test input") {
      assert(parse("[[1,2],[[3,4],5]]").magnitude)(equalTo(143)) &&
      assert(parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude)(equalTo(1384)) &&
      assert(parse("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude)(equalTo(445)) &&
      assert(parse("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude)(equalTo(791)) &&
      assert(parse("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude)(equalTo(1137)) &&
      assert(parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude)(equalTo(3488)) &&
      assert(parse("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]").magnitude)(equalTo(4140))
    },
    test("magnitude of sum should return correct magnitude for real input") {
      for {
        input <- ZFileReader.readLines("day-18-input-william.txt")(parse)
      } yield assert(sum(input).magnitude)(equalTo(4347))
    },
    test("largestMagnitudeSumOfTwo should return correct result for test input") {
      for {
        input <- ZFileReader.readLines("day18-test-input-2.txt")(parse)
      } yield assert(largestMagnitudeSumOfTwo(input))(equalTo(3993))
    },
    test("largestMagnitudeSumOfTwo should return correct result for real input") {
      for {
        input <- ZFileReader.readLines("day-18-input-william.txt")(parse)
      } yield assert(largestMagnitudeSumOfTwo(input))(equalTo(4721))
    },
  )
}
