package nl.njtromp.adventofcode

class Day02 extends Puzzle[Long] {
  private def findInvalidIds(range: String, validator: Long => Boolean): Seq[Long] =
    val idRange = LongRange(range.split('-').head.toLong, range.split('-').last.toLong)
    idRange.values().filter(validator.apply)

  override def exampleAnswerPart1: Long = 1227775554L
  override def solvePart1(lines: List[String]): Long =
    def isInvalid(id: Long): Boolean =
      val asString = id.toString
      asString.substring(0, asString.length / 2) == asString.substring(asString.length /  2)
    lines.head.split(',').flatMap(findInvalidIds(_, isInvalid)).sum

  override def exampleAnswerPart2: Long = 4174379265L
  override def solvePart2(lines: List[String]): Long =
    def isInvalid(id: Long): Boolean =
      val asString = id.toString
      (1 to asString.length / 2).exists(l => 
        val pattern = asString.substring(0, l)
        pattern.repeat(asString.length / l) ==  asString
      )
    lines.head.split(',').flatMap(findInvalidIds(_, isInvalid)).sum

}

object Day02 extends App {
  new Day02().solvePuzzles()
}
