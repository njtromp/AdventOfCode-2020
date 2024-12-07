package nl.njtromp.adventofcode

class Day07 extends Puzzle[Long] {

  private def parse(line: String): (Long, List[Long]) =
    val result = line.split(':').head.toLong
    val parts = line.split(':').last.trim.split(' ').map(_.toLong).toList
    (result, parts)

  private def isSolvablePart1(equation: (Long, List[Long])): Boolean =
    def solve(numbers: List[Long]): List[Long] =
      if numbers.length <= 1 then
        numbers
      else
        solve((numbers.head + numbers.tail.head) ::numbers.drop(2)) ++
          solve((numbers.head * numbers.tail.head) :: numbers.drop(2))
    val possibleResults = solve(equation._2)
    possibleResults.contains(equation._1)

  private def isSolvablePart2(equation: (Long, List[Long])): Boolean =
    def solve(numbers: List[Long]): List[Long] =
      if numbers.length <= 1 then
        numbers
      else
        solve((numbers.head + numbers.tail.head) ::numbers.drop(2)) ++
          solve((numbers.head * numbers.tail.head) :: numbers.drop(2)) ++
          solve((numbers.head.toString + numbers.tail.head.toString).toLong :: numbers.drop(2))
    val possibleResults = solve(equation._2)
    possibleResults.contains(equation._1)

  override def exampleAnswerPart1: Long = 3749
  override def solvePart1(lines: List[String]): Long =
    val equations = lines.map(parse)
    equations.filter(isSolvablePart1).map(_._1).sum

  override def exampleAnswerPart2: Long = 11387
  override def solvePart2(lines: List[String]): Long =
    val equations = lines.map(parse)
    equations.filter(isSolvablePart2).map(_._1).sum

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
