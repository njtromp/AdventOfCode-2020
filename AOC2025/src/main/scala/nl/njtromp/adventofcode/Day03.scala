package nl.njtromp.adventofcode

class Day03 extends Puzzle[Long] {
  private def turnBatteriesOn(nrOfBatteries: Int, batteries: String): String =
    def maximizeBatteries(min: Int, max: Int): String =
      if min < max && min < batteries.length then
        val highestDigitIndex = batteries.substring(min, max).zipWithIndex.foldLeft(min)((higest, bi) =>
          if bi._1 > batteries(higest) then
            bi._2
          else
            higest
        )
        val rightSide = if highestDigitIndex >= min && highestDigitIndex + 1 < max then
           turnBatteriesOn(nrOfBatteries - 1, batteries.substring(highestDigitIndex + 1, max))
        else
          ""
        val leftSide = if rightSide.isEmpty then
          turnBatteriesOn(nrOfBatteries - 1, batteries.substring(min, highestDigitIndex))
        else
          turnBatteriesOn(nrOfBatteries - 1 - rightSide.length, batteries.substring(min, highestDigitIndex))
        leftSide + batteries(highestDigitIndex) + rightSide
      else
        ""
    if nrOfBatteries > 0  then
      maximizeBatteries(0, batteries.length)
    else
      ""

  override def exampleAnswerPart1: Long = 357L
  override def solvePart1(lines: List[String]): Long =
    lines.map(turnBatteriesOn(2, _).toLong).sum

  override def exampleAnswerPart2: Long = 3121910778619L
  override def solvePart2(lines: List[String]): Long =
    lines.map(turnBatteriesOn(12, _).toLong).sum

}

object Day03 extends App {
  new Day03().solvePuzzles()
}
