package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.{LongRange, Puzzle2}
import nl.njtromp.adventofcode_2018.Day23.{sizeOfPart1, startOfPart2}

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class Day23 extends Puzzle2 {
  private type Pos = (Long, Long, Long)
  private val POS = "pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)".r
  private case class NanoBot(x: Long, y: Long, z: Long, radius: Long) {
    def distance(other: NanoBot): Long = Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)
    def isInRange(pos: Pos): Boolean = Math.abs(x - pos._1) + Math.abs(y - pos._2) + Math.abs(z - pos._3) <= radius
    def isInRange(other: NanoBot): Boolean = distance(other) <= radius
    def xRange: LongRange = LongRange(x - radius, x + radius)
    def yRange: LongRange = LongRange(y - radius, y + radius)
    def zRange: LongRange = LongRange(z - radius, z + radius)
  }

  override def exampleAnswerPart1: Long = 7
  override def solvePart1(lines: List[String]): Long = {
    val bots = lines.take(sizeOfPart1).map {
      case POS(x, y, z, r) => NanoBot(x.toLong, y.toLong, z.toLong, r.toLong)
    }
    sizeOfPart1 = Int.MaxValue
    val strongestBot = bots.maxBy(_.radius)
    bots.count(strongestBot.isInRange)
  }

  private def countOverlapping(ranges: List[LongRange]): List[(LongRange, Long)] =
    ranges.map(r => (r, ranges.count(_.isOverlapping(r))))

  @tailrec
  private def findCandidates(bots: List[NanoBot]): List[NanoBot] = {
    val xRanges = countOverlapping(bots.map(_.xRange))
    val xMax = xRanges.maxBy(_._2)._2
    val xs = xRanges.filter(_._2 == xMax).map(_._1)
    val xr = xs.fold(LongRange(Long.MinValue, Long.MaxValue))(_.intersect(_))

    val yRanges = countOverlapping(bots.map(_.yRange))
    val yMax = yRanges.maxBy(_._2)._2
    val ys = yRanges.filter(_._2 == yMax).map(_._1)
    val yr = ys.fold(LongRange(Long.MinValue, Long.MaxValue))(_.intersect(_))

    val zRanges = countOverlapping(bots.map(_.zRange))
    val zMax = zRanges.maxBy(_._2)._2
    val zs = zRanges.filter(_._2 == zMax).map(_._1)
    val zr = zs.fold(LongRange(Long.MinValue, Long.MaxValue))(_.intersect(_))

    val reducedBots = bots.filter(b => xr.isOverlapping(b.xRange) && yr.isOverlapping(b.yRange) && zr.isOverlapping(b.zRange))
    if (reducedBots.size == bots.size)
      bots
    else
      findCandidates(reducedBots)
  }

  private def findBestPos(bots: List[NanoBot], minX: Long, maxX: Long, minY: Long, maxY: Long, minZ: Long, maxZ: Long): (Pos, Long) = {
    var maxBots = 0
    var bestPos = (0L, 0L, 0L)
    for (x <- minX to maxX) {
      for (y <- minY to maxY) {
        for (z <- minZ to maxZ) {
          val pos = (x, y, z)
          val botCount = bots.count(_.isInRange(pos))
          if (botCount > maxBots) {
            maxBots = botCount
            bestPos = pos
            println(s"$pos -> $maxBots")
          }
        }
      }
    }
    (bestPos, maxBots)
  }

  override def exampleAnswerPart2: Long = 36
  override def solvePart2(lines: List[String]): Long = {
    val bots = lines.drop(startOfPart2).map {
      case POS(x, y, z, r) => NanoBot(x.toLong, y.toLong, z.toLong, r.toLong)
    }
    startOfPart2 = 0

    val candidateBots = findCandidates(bots)
    val minX = candidateBots.minBy(_.x).x
    val maxX = candidateBots.maxBy(_.x).x
    val midX = (minX + maxX) / 2
    val minY = candidateBots.minBy(_.y).y
    val maxY = candidateBots.maxBy(_.y).y
    val midY = (minY + maxY) / 2
    val minZ = candidateBots.minBy(_.z).z
    val maxZ = candidateBots.maxBy(_.z).z
    val midZ = (minZ + maxZ) / 2
    val bestPosAndCounts = List(
      Future(findBestPos(candidateBots, minX, midX, minY, midY, minZ, midZ)),
      Future(findBestPos(candidateBots, midX, maxX, minY, midY, minZ, midZ)),
      Future(findBestPos(candidateBots, minX, midX, midY, maxY, minZ, midZ)),
      Future(findBestPos(candidateBots, midX, maxX, midY, maxY, minZ, midZ)),
      Future(findBestPos(candidateBots, minX, midX, minY, midY, midZ, maxZ)),
      Future(findBestPos(candidateBots, midX, maxX, minY, midY, midZ, maxZ)),
      Future(findBestPos(candidateBots, minX, midX, midY, maxY, midZ, maxZ)),
      Future(findBestPos(candidateBots, midX, maxX, midY, maxY, midZ, maxZ))
    )

    do {
      Thread.sleep(1000)
    } while (!bestPosAndCounts.forall(_.isCompleted))

    val bestPos = bestPosAndCounts.map(_.value.get.get).maxBy(_._2)._1
    println(s"\nTHE BEST POST: $bestPos => ${bestPos._1 + bestPos._2 + bestPos._3}\n")

    bestPos._1 + bestPos._2 + bestPos._3
  }

}

object Day23 extends App {
  var sizeOfPart1 = 9;
  var startOfPart2 = 9;
  new Day23().solvePuzzles("/2018/day23.txt")
}
