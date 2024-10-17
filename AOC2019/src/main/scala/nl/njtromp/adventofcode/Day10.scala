package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day10 extends Puzzle[Long] {
  private type Pos = (Int, Int)
  private val ASTEROID = '#'

  private def locateAsteroids(lines: List[String]): List[Pos] =
    lines.zipWithIndex.flatMap(r => r._1.zipWithIndex.filter(_._1 == ASTEROID).map(a => (a._2, r._2)))

  private def findAsteroidsInSight(station: (Int, Int), asteroids: List[(Int, Int)]): List[(Pos, Rational)] =
    asteroids
      .filter(_ != station)
      // Get the 'delta' from the station to this asteroid
      .map(asteroid => (asteroid, Rational(station._2 - asteroid._2, asteroid._1 - station._1)))
      .sortBy(_._2.toDouble) // In increasing distance

  private def dedupInSameLineOfSight(asteroids: List[(Pos, Rational)]): List[(Pos, Rational)] =
    asteroids match
      case Nil => Nil
      case a :: tail => a :: dedupInSameLineOfSight(tail.filter(_._2 !== a._2))

  private def findStationLocation(asteroids: List[Pos]): (Pos, List[(Pos, Rational)]) =
    // Determine per asteroid the 'directions' to the other asteroids
    asteroids.map(a => (a, findAsteroidsInSight(a, asteroids)))
      // Remove any hidden asteroids
      .map(a => (a._1, dedupInSameLineOfSight(a._2)))
      // Select the asteroid with the largest number of other asteroids in sight
      .maxBy(_._2.size)

  private def activateLaser(station: Pos, asteroids: List[Pos]): Pos =
    @tailrec
    def vaporizeAsteroids(toBeVaporized: Int, asteroids: List[Pos]): Pos =
      val canBeVaporized = dedupInSameLineOfSight(findAsteroidsInSight(station, asteroids))
        .sortBy(_._2.angle) // Sort clockwise
        .map(_._1) // Just the asteroids
      // How many can be vaporized
      val amountToVaporize: Int = Math.min(canBeVaporized.size, toBeVaporized)
      // The asteroid we are looking for is in sight
      if toBeVaporized - amountToVaporize == 0 then
        // There it is, just 1 location to the left
        canBeVaporized(amountToVaporize - 1)
      else
        // Remove as much as we can
        val willBeVaporized = canBeVaporized.take(amountToVaporize)
        vaporizeAsteroids(toBeVaporized - amountToVaporize, asteroids.filter(a => !willBeVaporized.contains(a)))
    vaporizeAsteroids(200, asteroids)

  private val stations = mutable.Queue.empty[Pos]

  override def exampleAnswerPart1: Long = 210
  override def solvePart1(lines: List[String]): Long = {
    val stationWithAsteroidsInSight = findStationLocation(locateAsteroids(lines))
    stations.enqueue(stationWithAsteroidsInSight._1)
    stationWithAsteroidsInSight._2.size
  }

  override def exampleAnswerPart2: Long = 802
  override def solvePart2(lines: List[String]): Long =
    val asteroids = locateAsteroids(lines)
    val station = stations.dequeue()
    val lastAsteroid = activateLaser(station, asteroids)
    lastAsteroid._1 * 100 + lastAsteroid._2

}

object Day10 extends App {
  new Day10().solvePuzzles()
}
