package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day12 extends Puzzle[Long] {
  private type XYZ = (Long, Long, Long)

  private def parseMoonSystem(lines: List[String]): List[XYZ] =
    lines.map {
      case s"<x=$x, y=$y, z=$z>" => (x.trim.toLong, y.trim.toLong, z.trim.toLong)
    }

  private def applyGravity(moon: (XYZ, XYZ), moons: List[(XYZ, XYZ)]): XYZ =
    def gravityPull(a: Long, b: Long): Long =
      if a < b then
        1L
      else if a > b then
        -1L
      else
        0L
    moons.foldLeft(moon._2)((a, s) =>
      (
        a._1 + gravityPull(moon._1._1, s._1._1),
        a._2 + gravityPull(moon._1._2, s._1._2),
        a._3 + gravityPull(moon._1._3, s._1._3)
      )
    )

  private def step(moonSystem: List[(XYZ, XYZ)]): List[(XYZ, XYZ)] =
    moonSystem.map(ms =>
      val velocity = applyGravity(ms, moonSystem.filter(_ != ms))
      ((ms._1._1 + velocity._1, ms._1._2 + velocity._2, ms._1._3 + velocity._3), velocity)
    )

  @tailrec
  private def simulate(moonSystem: List[(XYZ, XYZ)], iterations: Long): Long =
    def sumAbs(a: XYZ): Long = Math.abs(a._1) + Math.abs(a._2) + Math.abs(a._3)
    if iterations == 0 then
      moonSystem.map(ms => sumAbs(ms._1) * sumAbs(ms._2)).sum
    else
      simulate(step(moonSystem), iterations - 1)

  private def simulate(moons: List[XYZ]): Long =
    simulate(moons.map(m => (m, (0L, 0L, 0L))), 1000)

  private def findRepetition(moons: List[XYZ]): Long =
    val moonSystem: List[(XYZ, XYZ)] = moons.map(m => (m, (0L, 0L, 0L): XYZ))
    val positions = Array.fill[mutable.Map[XYZ, Long]](4)(mutable.Map.empty[XYZ, Long].withDefaultValue(0L))
    (1 until 4).foreach(planet => positions(planet)(moonSystem(planet)._1) += 1)
    @tailrec
    def repeat(moonSystem: List[(XYZ, XYZ)], count: Long): Long =
      if count == 0 then
        -1L
      else
        val newMoonSystem = step(moonSystem)
        (0 until 4).foreach(planet => positions(planet)(moonSystem(planet)._1) += 1)
        repeat(newMoonSystem, count - 1)
    val result = repeat(moonSystem, 10000)
    (0 until 4).foreach(planet => println(f"${positions(planet).keySet.size}%,d"))
    println("="*20)
    result

  // <- The second example only uses 100 iterations, hence 1940 is not what we expect. It is however the answer
  // with just 100 iterations
  override def exampleAnswerPart1: Long = 14645
  override def solvePart1(lines: List[String]): Long =
    simulate(parseMoonSystem(lines))

  override def exampleAnswerPart2: Long = 4686774924L
  override def solvePart2(lines: List[String]): Long =
    findRepetition(parseMoonSystem(lines))

  private var steps = 0L
  private def printMoonSystem(moonSystem: List[(XYZ, XYZ)]): Unit =
    println(s"After $steps steps:")
    moonSystem.foreach(println)
    steps += 1
    println

}

object Day12 extends App {
  new Day12().solvePuzzles()
}
