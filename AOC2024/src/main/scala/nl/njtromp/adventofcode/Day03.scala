package nl.njtromp.adventofcode

class Day03 extends Puzzle[Long] {
  private val MUL = "mul\\(\\d+,\\d+\\).*".r
  private val DO = "do\\(\\).*".r
  private val DONT = "don't\\(\\).*".r
  private var enabled = true

  private def extractMuls(line: String): Long =
    if line.isEmpty then
      0
    else if MUL.matches(line) then
      val openingBracket = line.indexOf('(')
      val closingBracket = line.indexOf(')')
      val numbers = line.substring(openingBracket + 1, closingBracket).split(',')
      numbers.head.toLong * numbers.last.toLong + extractMuls(line.substring(closingBracket + 1))
    else
      extractMuls(line.tail)

  private def extractMulsActive(line: String): Long =
    if line.isEmpty then
      0
    else if DO.matches(line) then
      enabled = true
      extractMulsActive(line.substring("do()".length))
    else if DONT.matches(line) then
      enabled = false
      extractMulsActive(line.substring("dont()".length))
    else if MUL.matches(line) then
      val openingBracket = line.indexOf('(')
      val closingBracket = line.indexOf(')')
      val result = if enabled then
        val numbers = line.substring(openingBracket + 1, closingBracket).split(',')
        numbers.head.toLong * numbers.last.toLong
      else
        0
      result + extractMulsActive(line.substring(closingBracket + 1))
    else
      extractMulsActive(line.tail)

  override def exampleAnswerPart1: Long = 161
  override def solvePart1(lines: List[String]): Long =
    lines.map(extractMuls).sum

  // Very sneaky, the example doesn't contain any do()'s or dont()'s so the result of it still is 161!
  override def exampleAnswerPart2: Long = 161
  override def solvePart2(lines: List[String]): Long =
    enabled = true
    lines.map(extractMulsActive).sum

}

object Day03 extends App {
  new Day03().solvePuzzles()
}
