package nl.njtromp.adventofcode

class Day04 extends Puzzle[Long] with SimpleMapTypes {

  private def countWord(xmas: String, map: SimpleMap[Char]): Long =
    map.find(xmas.head)
      .flatMap(x => ALL_DIRECTIONS.map(d => map.getElements(x, d, xmas.length).mkString))
      .count(_ == xmas)

  private def countXWord(mas: String, map: SimpleMap[Char]): Long = {
    val halfLength = mas.length / 2
    map.find(_ == mas(halfLength))
      .count(x =>
        DIAGONAL.filter(d => map.isOnMap(map.moveOpposite(x, d, halfLength)))
          .map(d => map.getElements(map.moveOpposite(x, d, halfLength), d, mas.length).mkString)
          .count(_ == mas) == 2
      )
  }

  override def exampleAnswerPart1: Long = 18
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap[Char](lines, _.toCharArray)
    countWord("XMAS", map)

  override def exampleAnswerPart2: Long = 9
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap[Char](lines, _.toCharArray)
    countXWord("MAS", map)

}

object Day04 extends App {
  new Day04().solvePuzzles()
}
