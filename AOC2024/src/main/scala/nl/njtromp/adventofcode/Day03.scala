package nl.njtromp.adventofcode

class Day03 extends ParserPuzzle[Long] {
  private val MUL = "mul\\(\\d+,\\d+\\).*".r
  private val DO = "do()"
  private val DONT = "don't()"
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
    else if line.startsWith(DO) then
      enabled = true
      extractMulsActive(line.substring(DO.length))
    else if line.startsWith(DONT) then
      enabled = false
      extractMulsActive(line.substring(DONT.length))
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
    if lines.length == 2 then
      lines.take(1).map(extractMuls).sum
    else
      lines.map(extractMuls).sum

  override def exampleAnswerPart2: Long = 48
  override def solvePart2(lines: List[String]): Long =
    enabled = true
    if lines.length == 2 then
      lines.drop(1).map(extractMulsActive).sum
    else
      lines.map(extractMulsActive).sum

}

object Day03 extends App {
  new Day03().solvePuzzles()
}
