package nl.njtromp.adventofcode

class Day03 extends Puzzle[Long] {

  private def priority(item: Char): Long =
    if (item <= 'Z')
      item - 'A' + 27
    else
      item - 'a' + 1

  private def findMisplacedItem(content: String): Char =
    val firstHalf = content.substring(0, content.length / 2).toSet
    val secondHalf = content.substring(content.length / 2).toSet
    (firstHalf intersect secondHalf).head

  override def exampleAnswerPart1: Long = 157
  override def solvePart1(lines: List[String]): Long = {
    lines.map(findMisplacedItem).map(priority).sum
  }

  private def findBadge(rucksacks: List[String]): Char  =
    rucksacks.map(_.toSet).reduce(_ intersect _).head

  override def exampleAnswerPart2: Long = 70
  override def solvePart2(lines: List[String]): Long = {
    lines.grouped(3).map(findBadge).map(priority).sum
  }

}

object Day03 extends App{
  new Day03().solvePuzzles("/day03.txt")
}
