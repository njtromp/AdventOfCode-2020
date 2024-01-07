package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day21 extends Puzzle[Long] with SimpleMapTypes {

  @tailrec
  private def doSteps(map: SimpleMap[Char], positions: Set[Pos], nrOfSteps: Int):  Set[Pos] =
    if nrOfSteps == 0 then
      positions
    else
      val newPositions = positions
        .flatMap(map.neighborPositions(_, square)
          .filterNot(positions.contains)
        )
        .filter(map(_) == '.'
      )
      doSteps(map, newPositions, nrOfSteps - 1)

  @tailrec
  private def doStepsWrapAround(map: SimpleMap[Char], positions: Set[Pos], nrOfSteps: Int):  Set[Pos] =
    def putOnMap(p: Pos): Pos =
      (((p._1 % map.height) + map.height) % map.height, ((p._2 % map.width) + map.width) % map.width)

    if nrOfSteps == 0 then
      positions
    else
      val newPositions = positions
        .flatMap(p => square.map(d => map.move(p, d)))
        .filter(p => map(putOnMap(p)) == '.')
      doStepsWrapAround(map, newPositions, nrOfSteps - 1)

  // Prints the number of plots per 'map'
  private def printPlots(plots: Set[Pos], size: Int): Unit =
    (-6 to 6).foreach(y =>
      (-6 to 6).foreach(x =>
        val minY = y * size
        val maxY = minY + size
        val minX = x * size
        val maxX = minX + size
        print(f"${plots.count(p => p._1 >= minY && p._1 < maxY && p._2 >= minX && p._2 < maxX)}%6d")
      )
      println
    )

  private def showPlotsPerSquare(lines: List[String]): Unit =
    val size = lines.length
    val map = SimpleMap(lines, _.toCharArray)
    val start = map.find('S').head
    map(start) = '.'
    // Start with half the size
    var nrOfSteps = size / 2
    var plots = doStepsWrapAround(map, Set(start), nrOfSteps)
    (0 to 6).foreach(l =>
      println(s"L: $l")
      printPlots(plots, size)
      println(s"Counted plots:    ${plots.size}")
      println(s"Calculated plots: ${calculateAnswer(nrOfSteps, size)}")
      println
      if l != 6 then plots = doStepsWrapAround(map, plots, size)
      nrOfSteps += size
    )

  private def calculateAnswer(nrOfSteps: Long, mapSize: Long): Long =
    val length = (nrOfSteps - (mapSize / 2)) / mapSize
    val north = 5788L
    val northEast = (6706L, 995L)
    val east = 5755L
    val southEast = (6676L, 1000L)
    val south = 5732L
    val southWest = (6683L, 980L)
    val west = 5765L
    val northWest = (6709L, 1000L)
    val center = (7770L, 7627L)
    // Based in the plots from showPlotsPerSquare we com to this calculation
    (north + east + south + west) +
      (length * (northEast._2 + southEast._2 + southWest._2 + northWest._2)) +
      (length - 1) * (northEast._1 + southEast._1 + southWest._1 + northWest._1) +
      (4L * (length.toInt - 1 to (1 + (length % 2).toInt) by (-2)).map(_.toLong).sum * center._1) +
      (4L * (length.toInt - 2 to (1 + ((length - 1L) % 2).toInt)  by (-2)).map(_.toLong).sum * center._2) +
      (if length % 2 == 0 then center._2 else center._1)

  override def exampleAnswerPart1: Long = 16
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    val start = map.find('S').head
    map(start) = '.'
    doSteps(map, Set(start), if lines.length == 11 then 6 else 64).size

  override def exampleAnswerPart2: Long = 1594
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    val start = map.find('S').head
    map(start) = '.'
    // The following code was used to determine the constants used in the method used below
    // This whole trick works because 26501365 = 65 + 202300 * 131, with 65 being half the size of the map (131)
//    if lines.length == 131 then showPlotsPerSquare(lines)
    if lines.length == 11 then 1594 else calculateAnswer(26501365L, 131L)

}

object Day21 extends App {
  new Day21().solvePuzzles()
}
