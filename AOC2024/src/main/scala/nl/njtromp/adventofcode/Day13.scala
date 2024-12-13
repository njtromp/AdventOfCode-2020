package nl.njtromp.adventofcode

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, LUDecomposition}

class Day13 extends Puzzle[Long] {

  type Pos = (Long, Long)

  case class Button(costs: Long, dx: Long, dy: Long)

  case class Machine(a: Button, b: Button, prize: Pos) {
    private def checkPresses(aPressed: Double, bPressed: Double): Long =
      val a1 = Math.round(aPressed)
      val b1 = Math.round(bPressed)
      if a1 * a.dx + b1 * b.dx == prize._1 && a1 * a.dy + b1 * b.dy == prize._2 then
        a1 * a.costs + b1 * b.costs
      else
        0
    def play: Long =
      val matrix = new Array2DRowRealMatrix(Array(Array(a.dx.toDouble, b.dx.toDouble), Array(a.dy.toDouble, b.dy.toDouble)))
      val target = new ArrayRealVector(Array(prize._1.toDouble, prize._2.toDouble), false)
      val solution = new LUDecomposition(matrix).getSolver.solve(target)
      val aPresses = solution.getEntry(0)
      val bPresses = solution.getEntry(1)
      checkPresses(aPresses, bPresses)
  }

  private def parse(lines: List[String]): Machine =
    def parse(line: String): Button =
      line match
        case s"Button $i: X+$x, Y+$y" => Button(if i == "A" then 3 else 1, x.toInt, y.toInt)
    Machine(
      parse(lines.head),
      parse(lines(1)),
      lines.last match
        case s"Prize: X=$x, Y=$y" => (x.toInt, y.toInt)
    )

  override def exampleAnswerPart1: Long = 480
  override def solvePart1(lines: List[String]): Long =
    val slotMachines = groupByEmptyLine(lines).map(parse)
    slotMachines.map(_.play).sum

  // The expected values is from running the example once
  override def exampleAnswerPart2: Long = 875318608908L
  override def solvePart2(lines: List[String]): Long =
    val slotMachines = groupByEmptyLine(lines).map(parse).map(m => Machine(m.a, m.b, (10000000000000L + m.prize._1, 10000000000000L + m.prize._2)))
    slotMachines.map(_.play).sum

}

object Day13 extends App {
  new Day13().solvePuzzles()
}
