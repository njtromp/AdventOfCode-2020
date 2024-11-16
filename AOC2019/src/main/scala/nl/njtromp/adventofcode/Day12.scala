package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day12 extends Puzzle[Long] {
  // Position, Speed
  case class PS(x: Long, y: Long, z: Long)
  case class Moon(pos: PS, speed: PS)

  private def parseMoonSystem(lines: List[String]): List[Moon] =
    lines.map {
      case s"<x=$x, y=$y, z=$z>" => Moon(PS(x.trim.toLong, y.trim.toLong, z.trim.toLong), PS(0, 0, 0))
    }

  private def applyGravity(moon: Moon, moons: List[Moon]): PS =
    def gravityPull(a: Long, b: Long): Long =
      if a < b then
        1L
      else if a > b then
        -1L
      else
        0L
    moons.foldLeft(moon.speed)((a, s) =>
      PS(
        a.x + gravityPull(moon.pos.x, s.pos.x),
        a.y + gravityPull(moon.pos.y, s.pos.y),
        a.z + gravityPull(moon.pos.z, s.pos.z)
      )
    )

  private def step(moonSystem: List[Moon]): List[Moon] =
    moonSystem.map(ms =>
      val velocity = applyGravity(ms, moonSystem.filter(_ != ms))
      Moon(PS(ms.pos.x + velocity.x, ms.pos.y + velocity.y, ms.pos.z + velocity.z), velocity)
    )

  @tailrec
  private def simulate(moonSystem: List[Moon], iterations: Long): Long =
    def sumAbs(a: PS): Long = Math.abs(a.x) + Math.abs(a.y) + Math.abs(a.z)
    if iterations == 0 then
      moonSystem.map(ms => sumAbs(ms.pos) * sumAbs(ms.speed)).sum
    else
      simulate(step(moonSystem), iterations - 1)

  private def simulate(moons: List[Moon]): Long =
    simulate(moons, 1000)

  private def findRepetition(startingSystem: List[Moon], extractor: Moon => Long): Long =
    @tailrec
    def repeat(moonSystem: List[Moon], iterationCount: Long): Long =
      val newSystem = step(moonSystem)
      if newSystem.map(extractor).forall(_ == 0) then
        iterationCount
      else
        repeat(newSystem, iterationCount + 1)
    repeat(startingSystem, 1)

  // The second example only uses 100 iterations and for part 1 we need 1000 iteration, hence 1940 is not what we expect
  // 14645 is what we expect for 1000 iterations
  override def exampleAnswerPart1: Long = 14645
  override def solvePart1(lines: List[String]): Long =
    simulate(parseMoonSystem(lines))

  override def exampleAnswerPart2: Long = 4686774924L
  override def solvePart2(lines: List[String]): Long =
    // Thanks to: https://github.com/maneatingape/advent-of-code-scala/
    val startingSystem = parseMoonSystem(lines)
    val halfPeriodX = findRepetition(startingSystem, (m: Moon) => m.speed.x)
    val halfPeriodY = findRepetition(startingSystem, (m: Moon) => m.speed.y)
    val halfPeriodZ = findRepetition(startingSystem, (m: Moon) => m.speed.z)
    val halfPeriodXY = halfPeriodX * halfPeriodY / gcd(halfPeriodX, halfPeriodY)
    2 * halfPeriodXY * halfPeriodZ / gcd(halfPeriodXY, halfPeriodZ)

}

object Day12 extends App {
  new Day12().solvePuzzles()
}
