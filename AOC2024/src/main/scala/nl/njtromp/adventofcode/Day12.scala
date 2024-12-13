package nl.njtromp.adventofcode

import scala.collection.mutable

class Day12 extends Puzzle[Long] with SimpleMapTypes {

  private val inRegion = mutable.Set.empty[Pos]
  private def findRegion(start: Pos, map: SimpleMap[Char]): List[Pos] =
    def findRegion(current: Pos): List[Pos] =
      if inRegion.contains(current) then
        List.empty
      else
        inRegion += current
        val neighbors = map.neighborPositions(current, SQUARE)
          .filter(map(_) == map(current))
          .filterNot(inRegion.contains)
        if neighbors.isEmpty then
          List(current)
        else
          current :: neighbors.flatMap(findRegion)
    findRegion(start)

  private def findRegions(map: SimpleMap[Char]): List[List[Pos]] =
    inRegion.clear()
    val neighbors = map.allPositions()
    neighbors.map(start => findRegion(start, map))

  private def neighbors(region: List[Pos], map: SimpleMap[Char]): List[Pos] =
    region.toSet.flatMap(map.neighborPositions(_, SQUARE)).removedAll(region).toList

  private def isIsland(region: List[Pos], map: SimpleMap[Char]): Boolean =
    // Check that the 'island' is not on the edge of the map
    region.forall(p => SQUARE.forall(d => map.isOnMap(p + d))) &&
      region.toSet.flatMap(map.neighborPositions(_, ALL_DIRECTIONS))
        .removedAll(region)
        .map(map(_)).size == 1

  private def perimeter(region: List[Pos], map: SimpleMap[Char]): Long =
    region.map(p => 4 - map.neighbors(p, SQUARE).count(_ == map(p))).sum

  private def sides(region: List[Pos], islands: List[List[Pos]], map: SimpleMap[Char]): Long =
    val turnAfterMove = Map(RIGHT -> UP, UP -> LEFT, LEFT -> DOWN, DOWN -> RIGHT)
    val justTurn = Map(RIGHT -> DOWN, DOWN -> LEFT, LEFT -> UP, UP -> RIGHT)
    val checked = mutable.Set.empty[(Pos, Delta)]  // Used to o prevent endless loops
    def countSides(current: Pos, dir: Delta): Long =
      if checked.contains(current, dir) then
        0 // Back where we started so sides are known
      else
        checked += ((current, dir))
        val newPos = current + dir // Move one step and check if we're at a corner
        if region.contains(newPos) then
          val newDir = turnAfterMove(dir)
          if region.contains(newPos + newDir) then
            1 + countSides(newPos + newDir, newDir) // Yes, and try to turn at next move
          else
            countSides(newPos, dir) // No, just continue
        else
          1 + countSides(current, justTurn(dir))
    val outerSides = countSides(region.min, RIGHT)
    val innerSides = islands.filter(neighbors(_, map).toSet.diff(region.toSet).isEmpty)
      .map(island => sides(island, islands, map)).sum
    outerSides + innerSides

  override def exampleAnswerPart1: Long = 1930
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines)
    val regions = findRegions(map).filter(_.nonEmpty)
    regions.map(region => region.size * perimeter(region, map)).sum

  override def exampleAnswerPart2: Long = 1206
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines)
    val regions = findRegions(map).filter(_.nonEmpty)
    val islands = regions.filter(region => isIsland(region, map))
    regions.map(region => region.size * sides(region, islands, map)).sum

}

object Day12 extends App {
  new Day12().solvePuzzles()
}
