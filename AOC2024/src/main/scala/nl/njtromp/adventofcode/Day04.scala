package nl.njtromp.adventofcode

class Day04 extends Puzzle[Long] with SimpleMapTypes {

  private def findWord(xmas: String, map: SimpleMap[Char]): Long =
    def createWord(x: Pos, d: Delta, length: Int): String =
      if length == 0 || !map.isOnMap(x) then
        ""
      else
        val m = map.move(x, d)
        map(x) + createWord(m, d, length - 1)
    map.find(xmas.head).flatMap(x =>
      all.map(d => createWord(x, d, xmas.length)))
      .count(_ == xmas
    )

  private def findX_MAS(map: SimpleMap[Char]): Long =
    def createMAS(a: Pos, d: Delta): String =
      val m = map.move(a, d)
      val s = map.moveOpposite(a, d)
      if map.isOnMap(m) && map.isOnMap(s) then
        s"${map(m)}A${map(s)}"
      else
        ""
    def isX_MAS(a: (Int, Int), deltas: List[(Int, Int)]): Boolean =
      deltas.count(d => List("MAS", "SAM").contains(createMAS(a, d))) == 2
    map.find(_ == 'A').count(isX_MAS(_, List(upRight, downRight)))

  override def exampleAnswerPart1: Long = 18
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap[Char](lines, _.toCharArray)
    findWord("XMAS", map)

  override def exampleAnswerPart2: Long = 9
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap[Char](lines, _.toCharArray)
    findX_MAS(map)

}

object Day04 extends App {
  new Day04().solvePuzzles()
}
