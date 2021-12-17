package day16

object Solution {

  sealed trait Packet {
    def packetTypeId: Int
    def bitLength: Int
  }

  case class Literal(version: Int, value: Long, bitLength: Int) extends Packet {
    val packetTypeId = 4
  }

  case class Operator(
      version: Int,
      packetTypeId: Int,
      lengthType: LengthType,
      bitLength: Int,
      subPackets: Seq[Packet]
  ) extends Packet

  sealed trait LengthType
  case class Zero(subPacketsBitLength: Int) extends LengthType
  case class One(numSubPackets: Int)        extends LengthType

  object Packet {
    def apply(transmission: String): Packet = {
      //   println(transmission.grouped(4).toSeq)
      val version    = Integer.parseInt(transmission.take(3), 2)
      val packetType = Integer.parseInt(transmission.slice(3, 6), 2)
      if (packetType == 4) {
        val digitBits = transmission.slice(6, transmission.size)
        var stop      = digitBits.head
        var cursor    = 0
        while (stop == '1') {
          cursor += 5
          stop = digitBits(cursor)
        }
        cursor += 5
        Literal(version, 0L, cursor + 6)
      } else {
        val lengthTypeId = Integer.parseInt(transmission.slice(6, 7), 2)
        if (lengthTypeId == 0) {
          val length = Integer.parseInt(transmission.slice(7, 22), 2)
          Operator(
            version,
            packetType,
            Zero(length),
            3 + 3 + 1 + 15 + length,
            readAllPacketsWithBitLength(transmission.drop(22), length)
          )
        } else {
          val numSubPackets = Integer.parseInt(transmission.slice(7, 18), 2)
          val subPackets    = readAllPacketsWithNumPackets(transmission.drop(18), numSubPackets)
          Operator(
            version,
            packetType,
            One(numSubPackets),
            3 + 3 + 1 + 11 + subPackets.map(_.bitLength).sum,
            subPackets
          )
        }

      }
    }

    def readAllPacketsWithBitLength(input: String, bitLength: Int): Seq[Packet] = {
      var ret: Seq[Packet] = Seq.empty
      var cursor           = 0
      while (cursor < bitLength) {
        val current = Packet(input.drop(cursor))
        ret = ret :+ current
        cursor += current.bitLength
      }
      ret
    }

    def readAllPacketsWithNumPackets(input: String, numPackets: Int): Seq[Packet] = {
      var cursor = 0
      for {
        _ <- 0 until numPackets
      } yield {
        val p = Packet(input.drop(cursor))
        cursor += p.bitLength
        p
      }
    }
  }

  def parseTransmission(line: String): String = {
    val sb = new StringBuilder()
    line.strip().foreach { c =>
      val b = Integer.parseInt(c.toString(), 16).toBinaryString.reverse.padTo(4, '0').reverse
      sb.append(b)
    }
    sb.toString()
  }

  def getSumOfVersions(transmission: String): Int = {
    val packet = Packet.apply(transmission)
    def helper(p: Packet): Int = p match {
      case Literal(version, _, _)                 => version
      case Operator(version, _, _, _, subPackets) => version + subPackets.map(helper).sum
    }
    helper(packet)
  }

}
