package nl.njtromp.adventofcode

class Day10B extends Puzzle[Long] with SimpleMapTypes {

  private def findTrail(current: Pos, map: SimpleMap[Int]): List[Pos] =
    if map(current) == 9 then
      List(current)
    else
      map.neighborPositions(current, SQUARE)
        .filter(map(_) == map(current) + 1)
        .flatMap(findTrail(_, map))

  override def exampleAnswerPart1: Long = 36
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray.map(_.asDigit))
    map.find(0).map(findTrail(_, map).toSet.size).sum

  override def exampleAnswerPart2: Long = 81
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray.map(_.asDigit))
    map.find(0).map(findTrail(_, map).size).sum

}

object Day10B extends App {
  new Day10B().solvePuzzles("/day10.txt")
}
