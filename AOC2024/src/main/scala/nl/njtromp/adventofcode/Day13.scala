package nl.njtromp.adventofcode

class Day13 extends Puzzle[Long] with LinearAlgebra {

  type Pos = (Long, Long)

  case class Button(costs: Long, dx: Long, dy: Long)

  case class Machine(a: Button, b: Button, prize: Pos) {
    def play: Long =
      solve(Array(Array(a.dx, b.dx), Array(a.dy, b.dy)), Array(prize._1, prize._2)) match
        case Some(s) => s.zip(Array(a.costs, b.costs)).map(_ * _).sum
        case None => 0
  }

  private def parse(lines: List[String]): Machine =
    def parse(line: String): Button =
      line match
        case s"Button $t: X+$x, Y+$y" => Button(if t == "A" then 3 else 1, x.toLong, y.toLong)
    Machine(
      parse(lines.head),
      parse(lines(1)),
      lines.last match
        case s"Prize: X=$x, Y=$y" => (x.toLong, y.toLong)
    )

  override def exampleAnswerPart1: Long = 480
  override def solvePart1(lines: List[String]): Long =
    val slotMachines = groupByEmptyLine(lines).map(parse)
    slotMachines.map(_.play).sum

  // The expected values is from running the example once
  override def exampleAnswerPart2: Long = 875318608908L
  override def solvePart2(lines: List[String]): Long =
    val slotMachines = groupByEmptyLine(lines).map(parse)
      .map(m => Machine(m.a, m.b, (10000000000000L + m.prize._1, 10000000000000L + m.prize._2)))
    slotMachines.map(_.play).sum

}

object Day13 extends App {
  new Day13().solvePuzzles()
}
