package nl.njtromp.adventofcode

class Day01 extends Puzzle[Long] {
  private def calculateFuel(mass: Long) =
    mass / 3 - 2

  private def includeFuel(mass: Long): Long =
    val fuel = calculateFuel(mass)
    if fuel <= 0 then
      0
    else
      fuel + includeFuel(fuel)

  override def exampleAnswerPart1: Long = 2 + 2 + 654 + 33583
  override def solvePart1(lines: List[String]): Long =
    lines.map(l => calculateFuel(l.toLong)).sum

  override def exampleAnswerPart2: Long = 2 + 2 + 966 + 50346
  override def solvePart2(lines: List[String]): Long =
    lines.map(l => {
      val fuel = calculateFuel(l.toLong)
      fuel + includeFuel(fuel)
    }).sum

}

object Day01 extends App {
  new Day01().solvePuzzles()
}
