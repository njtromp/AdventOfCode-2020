package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day12 extends Puzzle[Long] {
  private type XYZ = (Long, Long, Long)

  private def simulate(moons: List[XYZ], iterations: Long): Long =
    def determineGravity(moon: (XYZ, XYZ), moons: List[(XYZ, XYZ)]): XYZ =
      moons.foldLeft((0L, 0L, 0L))((a, s) =>
        (
          a._1 + Math.signum(s._1._1 - moon._1._1).toLong,
          a._2 + Math.signum(s._1._2 - moon._1._2).toLong,
          a._3 + Math.signum(s._1._3 - moon._1._3).toLong
        )
      )
    @tailrec
    def simulate(moonSystem: List[(XYZ, XYZ)], iterations: Long): Long =
      def sum(a: XYZ): Long = Math.abs(a._1) + Math.abs(a._2) + Math.abs(a._3)
      if iterations == 0 then
        moonSystem.map(ms => sum(ms._1) * sum(ms._2)).sum
      else
        simulate(
          moonSystem.map(ms =>
            val gravity = determineGravity(ms, moonSystem.filterNot(_ == _))
            val velocity = (ms._2._1 + gravity._1, ms._2._2 + gravity._2, ms._2._3 + gravity._3)
            ((ms._1._1 + velocity._1, ms._1._2 + velocity._2, ms._1._3 + velocity._3), velocity)
          ), iterations - 1
        )
    simulate(moons.map(m => (m, (0L, 0L, 0L))), iterations)

  override def exampleAnswerPart1: Long = 39413//179
  override def solvePart1(lines: List[String]): Long =
    val moons = lines.map {
      case s"<x=$x, y=$y, z=$z>" => (x.trim.toLong, y.trim.toLong, z.trim.toLong)
    }
    simulate(moons, 1000)

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day12 extends App {
  new Day12().solvePuzzles()
}
