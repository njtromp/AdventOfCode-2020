package nl.njtromp.adventofcode

class Day08 extends Puzzle[Long] with SimpleMapTypes {

  extension (p: Pos)
    def - (o: Pos): Delta = (o._1 - p._1, o._2 - p._2)
  extension (d: Delta)
    def * (l: Int): Pos = (l * d._1, l * d._2)

  private def markAntiNodes1(map: SimpleMap[Char]): Unit =
    def markAntiNodes(antennas: (Char, List[Pos])): Unit =
      val as = antennas._2.toSet
      val antiNodes = as.flatMap(a =>
        as.filterNot(_ == a).flatMap(b =>
          val d = a - b
          Set(
            map.move(a, d),
            map.moveOpposite(a, d),
            map.move(b, d),
            map.moveOpposite(b, d)
          )
            .filter(map.isOnMap) -- as
        )
      )
      antiNodes.foreach(map(_) = '#')
    val antennas = map.find(_ != '.').groupBy(map(_))
    antennas.foreach(markAntiNodes)

  private def markAntiNodes2(map: SimpleMap[Char]): Unit =
    def markAntinodes(antennas: (Char, List[Pos])): Unit =
      val as = antennas._2.toSet
      val antiNodes = as.flatMap(a =>
        as.filterNot(_ == a).flatMap(b =>
          val d = a - b
          (1 to 50).indices.flatMap(l => // The real map is 50x50 hence duplicating anti nodes 50 times
            Set(
              map.move(a, d * l),
              map.moveOpposite(a, d * l),
              map.move(b, d * l),
              map.moveOpposite(b, d * l)
            )
              .filter(map.isOnMap)
          )
        )
      )
      antiNodes.foreach(map(_) = '#')
    val antennas = map.find(_ != '.').groupBy(map(_))
    antennas.foreach(markAntinodes)

  override def exampleAnswerPart1: Long = 14
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines)
    markAntiNodes1(map)
    map.find('#').size

  override def exampleAnswerPart2: Long = 34
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines)
    markAntiNodes2(map)
    map.find('#').size

}

object Day08 extends App {
  new Day08().solvePuzzles()
}
