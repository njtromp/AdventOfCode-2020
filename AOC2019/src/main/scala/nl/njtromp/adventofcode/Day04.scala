package nl.njtromp.adventofcode

class Day04 extends Puzzle[Long] {

  private def isValidPasswordPart1(password: Int): Boolean =
    val asString = password.toString
    val pairs = asString.zip(asString.tail)
    pairs.forall(p => p._1 <= p._2) // Not decreasing
      && pairs.count(p => p._1 == p._2) > 0 // At least 2 same numbers
  
  private def isValidPasswordPart2(password: Int): Boolean =
    val asString = password.toString
    val pairs = asString.zip(asString.tail)
    pairs.forall(p => p._1 <= p._2) // Not decreasing
      && pairs.filter(p => p._1 == p._2) // Keep the pairs that are identical
      .groupBy(n => n) // Group then on their value
      .count(_._2.size == 1) >= 1 // There should be at least one pair that is not repeated

  private def numberOfValidPasswords(range: Array[String], predicate: Int => Boolean): Long =
    (range.head.toInt to range.last.toInt)
      .count(predicate)

  override def exampleAnswerPart1: Long = 2
  override def solvePart1(lines: List[String]): Long =
    numberOfValidPasswords(lines.head.split("-"), isValidPasswordPart1)

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    numberOfValidPasswords(lines.head.split("-"), isValidPasswordPart2)

}

object Day04 extends App {
  new Day04().solvePuzzles()
}
