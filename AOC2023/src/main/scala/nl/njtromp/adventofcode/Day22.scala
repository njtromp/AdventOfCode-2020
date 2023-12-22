package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day22 extends Puzzle[Long] {
  // (x, y, z)
  private type Pos = (Long, Long, Long)
  private type Delta = (Long, Long, Long)
  private val down = (0L, 0L, -1L)
  private val brick = "(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)".r
  private case class Brick(id: Char, begin: Pos, end: Pos) {
    def move(delta: Delta): Brick = Brick(
      id,
      (begin._1 + delta._1, begin._2 + delta._2, begin._3 + delta._3),
      (end._1 + delta._1, end._2 + delta._2, end._3 + delta._3)
    )
    lazy val minX: Long = Math.min(begin._1, end._1)
    lazy val minY: Long = Math.min(begin._2, end._2)
    lazy val minZ: Long = Math.min(begin._3, end._3)
    lazy val maxX: Long = Math.max(begin._1, end._1)
    lazy val maxY: Long = Math.max(begin._2, end._2)
    lazy val maxZ: Long = Math.max(begin._3, end._3)
    def overlapping(other: Brick): Boolean =
      minX <= other.maxX && maxX >= other.minX &&
      minY <= other.maxY && maxY >= other.minY
    def isSupporting(other: Brick): Boolean =
      maxZ + 1 == other.minZ && overlapping(other)
  }

  private def parseLine(line: String, id: Int): Brick =
    line match
      case brick(x1, y1, z1, x2, y2, z2) => Brick(
        ('A'.toInt + id).toChar,
        (x1.toLong, y1.toLong, z1.toLong),
        (x2.toLong, y2.toLong, z2.toLong)
      )

  private def settleBricks(bricks: List[Brick]): List[Brick] =
    @tailrec
    def settle(settled: List[Brick], unsettled: List[Brick]): List[Brick] =
      if unsettled.isEmpty then
        settled
      else
        val newlySettled = unsettled.filter(u => u.minZ == 1 || settled.exists(_.isSupporting(u)))
        if newlySettled.isEmpty then
          settle(settled, unsettled.map(_.move(down)))
        else
          settle(settled ++ newlySettled, unsettled.filterNot(newlySettled.contains(_)))
    settle(bricks.filter(_.minZ == 1), bricks.filter(_.minZ > 1))

  override def exampleAnswerPart1: Long = 5
  override def solvePart1(lines: List[String]): Long =
    val bricks = lines.zipWithIndex.map(parseLine).sortBy(_.minZ)
    val settledBricks = settleBricks(bricks)
    settledBricks.count(s => {
      val reduced = settledBricks.filterNot(_ == s)
      reduced.forall(r => r.minZ == 1 || reduced.exists(_.isSupporting(r)))
    })

  override def exampleAnswerPart2: Long = 7
  override def solvePart2(lines: List[String]): Long =
    val bricks = lines.zipWithIndex.map(parseLine).sortBy(_.minZ)
    val settledBricks = settleBricks(bricks)
    val brickSet = settledBricks.toSet
    settledBricks.map(s => (settleBricks(settledBricks.filterNot(_ == s)).toSet diff brickSet).size).sum

}

object Day22 extends App {
  new Day22().solvePuzzles()
}
