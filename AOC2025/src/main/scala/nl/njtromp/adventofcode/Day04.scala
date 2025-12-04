package nl.njtromp.adventofcode

class Day04 extends Puzzle[Long] with SimpleMapTypes {
  private val PAPER = '@'
  private val NO_PAPER = '.'

  override def exampleAnswerPart1: Long = 13
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines)
    map.allPositions()
      .filter(map(_) == PAPER)
      .count(p => map.neighbors(p, ALL_DIRECTIONS).count(_ == PAPER) < 4)


  override def exampleAnswerPart2: Long = 43
  override def solvePart2(lines: List[String]): Long =
    def removePaperRolls(map: SimpleMap[Char]): Long =
      val removablePaper = map.allPositions()
        .filter(map(_) == PAPER)
        .filter(p => map.neighbors(p, ALL_DIRECTIONS).count(_ == PAPER) < 4)
      if removablePaper.isEmpty then
        0
      else
        map.setAll(removablePaper, NO_PAPER)
        removablePaper.size + removePaperRolls(map)
    removePaperRolls(SimpleMap(lines))

}

object Day04 extends App {
  new Day04().solvePuzzles()
}
