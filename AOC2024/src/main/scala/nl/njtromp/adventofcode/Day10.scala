package nl.njtromp.adventofcode

class Day10 extends Puzzle[Long] with SimpleMapTypes {

  private def findTrails(starts: List[Pos], map: SimpleMap[Int], unique: Boolean): List[Pos] =
    def findTrail(current: Pos): List[Pos] =
      if map(current) == 9 then
        List(current)
      else
        val trail = map.neighborPositions(current, SQUARE).filter(map(_) == map(current) + 1)
        trail.flatMap(findTrail)
    starts.flatMap(s =>
      val trail = findTrail(s)
      if unique then
        trail.toSet
      else
        trail
    )

  override def exampleAnswerPart1: Long = 36
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray.map(_.asDigit))
    val startingPoints = map.find(0)
    findTrails(startingPoints, map, true).size

  override def exampleAnswerPart2: Long = 81
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray.map(_.asDigit))
    val startingPoints = map.find(0)
    findTrails(startingPoints, map, false).size

}

object Day10 extends App {
  new Day10().solvePuzzles()
}
