package day16

import zio.test._
import Assertion._
import common.ZFileReader
import day16.Solution._

object SolutionSpec extends DefaultRunnableSpec {
  def spec = suite("day16.SolutionSpec")(
    test("Packet.apply should return literal value for literal value transmission") {
      assert(Packet(parseTransmission("D2FE28")))(equalTo(Literal(6, 2021L, 21)))
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
              Literal(6, 10L, 11),
              Literal(2, 20L, 16)
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
              Literal(2, 1L, 11),
              Literal(4, 2L, 11),
              Literal(1, 3L, 11)
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
    },
    test("eval should return evaluated value for test input") {
      assert(eval(Packet(parseTransmission("D2FE28"))))(equalTo(2021L)) &&
      assert(eval(Packet(parseTransmission("38006F45291200"))))(equalTo(1L)) &&
      assert(eval(Packet(parseTransmission("C200B40A82"))))(equalTo(3L)) &&
      assert(eval(Packet(parseTransmission("04005AC33890"))))(equalTo(54L)) &&
      assert(eval(Packet(parseTransmission("880086C3E88112"))))(equalTo(7L)) &&
      assert(eval(Packet(parseTransmission("CE00C43D881120"))))(equalTo(9L)) &&
      assert(eval(Packet(parseTransmission("D8005AC2A8F0"))))(equalTo(1L)) &&
      assert(eval(Packet(parseTransmission("F600BC2D8F"))))(equalTo(0L)) &&
      assert(eval(Packet(parseTransmission("9C005AC2F8F0"))))(equalTo(0L)) &&
      assert(eval(Packet(parseTransmission("9C0141080250320F1802104A08"))))(equalTo(1L))
    },
    test("eval should evaluate value of real input") {
      for {
        bin <- ZFileReader.readLines("day-16-input-william.txt")(parseTransmission).head
      } yield assert(eval(Packet(bin)))(equalTo(158135423448L))
    }
  )
}
