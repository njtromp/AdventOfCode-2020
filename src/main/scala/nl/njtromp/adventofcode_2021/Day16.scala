package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day16 extends Puzzle {

  def parseHex(c: Char): Int = Integer.parseInt(s"$c", 16)
  def parseHex(s: String): Int = Integer.parseInt(s, 16)
  def parseBin(s: String): Int = Integer.parseInt(s, 2)
  def parseBigBin(s: String): BigInt = s.toCharArray.foldLeft(BigInt.apply(0))((acc, c) => acc * BigInt.apply(2) + (if (c == '1') BigInt.apply(1) else BigInt.apply(0)))
  def toBinString(d: Int): String = Integer.toString(d, 2)

  sealed abstract class Packet {
    def bits: Int
    def value: BigInt
  }
  case class Literal(version: Int, values: List[String]) extends Packet {
    def bits: Int = 3 + 3 + (1 + 4) * values.length
    def value: BigInt = parseBigBin(values.mkString)
  }
  case class Operation(version: Int, opCode: Int, length: Int, subPackets: List[Packet]) extends Packet {
    def bits: Int = 3 + 3 + 1 + length + subPackets.map(_.bits).sum
    def value: BigInt = opCode match {
      case 0 => subPackets.map(_.value).sum
      case 1 => subPackets.map(_.value).product
      case 2 => subPackets.map(_.value).min
      case 3 => subPackets.map(_.value).max
      case 5 => subPackets.map(_.value).reduce((a, b) => if (a > b) 1 else 0)
      case 6 => subPackets.map(_.value).reduce((a, b) => if (a < b) 1 else 0)
      case 7 => subPackets.map(_.value).reduce((a, b) => if (a == b) 1 else 0)
    }
  }

  def decodeLiteral(encodedMessage: String): List[String] = {
    if (encodedMessage.head == '0')
      List(encodedMessage.slice(1, 5))
    else
      encodedMessage.slice(1, 5) :: decodeLiteral(encodedMessage.drop(5))
  }

  private def decodePacket(encodedMessage: String): Packet = {
    val version = parseBin(encodedMessage.take(3))
    val typeId = parseBin(encodedMessage.slice(3, 6))
    val message = encodedMessage.drop(6)
    typeId match {
      case 4 =>
        val literals = decodeLiteral(message)
        Literal(version, literals)
      case _ =>
        message.head match {
          case '0' =>
            val packetSize = parseBin(message.slice(1,16))
            Operation(version, typeId, 15, decodePackets(message.slice(16, 16 + packetSize)))
          case '1' =>
            val numberOfPackets = parseBin(message.slice(1, 12))
            val packets = (1 to numberOfPackets).foldLeft(List.empty[Packet])((acc, _) =>
              decodePacket(message.drop(12 + acc.map(_.bits).sum)) :: acc
            ).reverse
            Operation(version, typeId, 11, packets)
        }
    }
  }

  private def decodePackets(encodedMessage: String): List[Packet] = {
    if (encodedMessage.length < 6) {
      List.empty[Packet]
    } else {
      val packet = decodePacket(encodedMessage)
      packet :: decodePackets(encodedMessage.drop(packet.bits))
    }
  }

  def sumVersionNumbers(p: Packet): Int = {
    p match {
      case Literal(v, _) => v
      case Operation(v, _, _, s) => v + s.map(sumVersionNumbers).sum
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val bitEncodedMessage = lines.head.map(parseHex).map(toBinString).map("000" + _ takeRight 4).toList.mkString
    val decodedPacket = decodePacket(bitEncodedMessage)
    sumVersionNumbers(decodedPacket)
  }

  override def solvePart2(lines: List[String]): Long = {
    val bitEncodedMessage = lines.head.map(parseHex).map(toBinString).map("000" + _ takeRight 4).toList.mkString
    val decodedPacket = decodePacket(bitEncodedMessage)
    decodedPacket.value.longValue
  }
}

object Day16 extends App {
  new Day16().solvePuzzles("/2021/day16.txt")
}
