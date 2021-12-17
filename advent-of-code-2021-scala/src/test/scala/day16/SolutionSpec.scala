package day16

import zio.test._
import Assertion._
import common.ZFileReader
import day16.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day16.SolutionSpec")(
    test("Packet.apply should return literal value for literal value transmission") {
      assert(Packet(parseTransmission("D2FE28")))(equalTo(Literal(6, 0L, 21)))
    },
    test("Packet.apply should return value for operator transmission") {
      assert(Packet(parseTransmission("38006F45291200")))(
        equalTo(
          Operator(
            version = 1,
            packetTypeId = 6,
            lengthType = Zero(27),
            bitLength = 49,
            Seq(
              Literal(6, 0L, 11),
              Literal(2, 0L, 16)
            )
          )
        )
      ) &&
      assert(Packet(parseTransmission("EE00D40C823060")))(
        equalTo(
          Operator(
            version = 7,
            packetTypeId = 3,
            lengthType = One(3),
            bitLength = 51,
            Seq(
              Literal(2, 0L, 11),
              Literal(4, 0L, 11),
              Literal(1, 0L, 11)
            )
          )
        )
      )
    },
    test("getSumOfVersions should get sum of versions for test input") {
      assert(getSumOfVersions(parseTransmission("38006F45291200")))(equalTo(9)) &&
      assert(getSumOfVersions(parseTransmission("8A004A801A8002F478")))(equalTo(16)) &&
      assert(getSumOfVersions(parseTransmission("620080001611562C8802118E34")))(equalTo(12)) &&
      assert(getSumOfVersions(parseTransmission("C0015000016115A2E0802F182340")))(equalTo(23)) &&
      assert(getSumOfVersions(parseTransmission("A0016C880162017C3686B18A3D4780")))(equalTo(31))
    },
    test("getSumOfVersions should get sum of versions for real input") {
      for {
        bin <- ZFileReader.readLines("day-16-input-william.txt")(parseTransmission).head
      } yield assert(getSumOfVersions(bin))(equalTo(955))
    }
  )
}
