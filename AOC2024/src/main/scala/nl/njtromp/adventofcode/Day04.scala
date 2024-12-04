package nl.njtromp.adventofcode

class Day04 extends Puzzle[Long] with SimpleMapTypes {

  private def countWord(word: String, map: SimpleMap[Char]): Long =
    map.find(word.head)
      .flatMap(x => ALL_DIRECTIONS.map(d => map.getElements(x, d, word.length).mkString))
      .count(_ == word)

  private def countXWord(word: String, map: SimpleMap[Char]): Long = {
    val halfLength = word.length / 2
    map.find(_ == word(halfLength))
      .count(x =>
        DIAGONAL.filter(d => map.isOnMap(map.moveOpposite(x, d, halfLength)))
          .map(d => map.getElements(map.moveOpposite(x, d, halfLength), d, word.length).mkString)
          .count(_ == word) == 2
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
