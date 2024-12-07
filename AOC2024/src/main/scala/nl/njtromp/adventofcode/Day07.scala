package nl.njtromp.adventofcode

class Day07 extends Puzzle[Long] {

  private def parse(line: String): (Long, List[Long]) =
    val result = line.split(':').head.toLong
    val parts = line.split(':').last.trim.split(' ').map(_.toLong).toList
    (result, parts)

  private def isSolvable(equation: (Long, List[Long]), ops: List[(Long, Long) => Long]): Boolean =
    def solve(numbers: List[Long]): List[Long] =
      if numbers.length <= 1 then
        numbers
      else
        ops.flatMap(op => solve(op(numbers.head, numbers(1)) :: numbers.drop(2)))
    solve(equation._2).contains(equation._1)

  override def exampleAnswerPart1: Long = 3749
  override def solvePart1(lines: List[String]): Long =
    val equations = lines.map(parse)
    val operations: List[(Long, Long) => Long] = List(
      (a, b) => a * b,
      (a, b) => a + b
    )
    equations.filter(isSolvable(_, operations)).map(_._1).sum

  override def exampleAnswerPart2: Long = 11387
  override def solvePart2(lines: List[String]): Long =
    val equations = lines.map(parse)
    val operations: List[(Long, Long) => Long] = List(
      (a, b) => a * b,
      (a, b) => a + b,
      (a, b) => s"$a$b".toLong
    )
    equations.filter(isSolvable(_, operations)).map(_._1).sum

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
