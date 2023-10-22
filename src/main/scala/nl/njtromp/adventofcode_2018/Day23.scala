package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

class Day23 extends Puzzle2 {
  private type Pos = (Long, Long, Long)
  private val POS = "pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)".r
  private case class NanoBot(x: Long, y: Long, z: Long, radius: Long) {
    def distance(other: NanoBot): Long = Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)
    def isInRange(other: NanoBot): Boolean = distance(other) <= radius
    def inRange: Set[Pos] = ???
  }

  override def exampleAnswerPart1: Long = 7
  override def solvePart1(lines: List[String]): Long = {
    val bots = lines.map {
      case POS(x, y, z, r) => NanoBot(x.toLong, y.toLong, z.toLong, r.toLong)
    }
    val strongestBot = bots.maxBy(_.radius)
    bots.count(strongestBot.isInRange)
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    val bots = lines.map {
      case POS(x, y, z, r) => NanoBot(x.toLong, y.toLong, z.toLong, r.toLong)
    }
    ???
  }

}

object Day23 extends App {
  new Day23().solvePuzzles("/2018/day23.txt")
}
