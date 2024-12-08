package nl.njtromp.adventofcode

class Day08 extends Puzzle[Long] with SimpleMapTypes {

  private def markAntiNodes(map: SimpleMap[Char], min: Int, max: Int): Set[Pos] =
    def markAntiNodes(antennas: (Char, List[Pos])): Set[Pos] =
      val as = antennas._2.toSet
      as.flatMap(a =>
        as.filterNot(_ == a).flatMap(b =>
          val d = a - b
          (min to max).flatMap(l =>
            Set(
              map.move(b, d, l),
              map.moveOpposite(a, d, l),
            )
              .filter(map.isOnMap)
          )
        )
      )
    val antennaGroups = map.find(_ != '.').groupBy(map(_))
    antennaGroups.toSet.flatMap(markAntiNodes)

  override def exampleAnswerPart1: Long = 14
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines)
    markAntiNodes(map, 1, 1).size

  override def exampleAnswerPart2: Long = 34
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines)
    markAntiNodes(map, 0, map.height).size

}

object Day08 extends App {
  new Day08().solvePuzzles()
}
