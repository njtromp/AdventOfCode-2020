package nl.njtromp.adventofcode

class Day18 extends Puzzle[Long] with SimpleMapTypes {
  private type Pos = (Int, Int)
  private val HORIZONTAL: Char = "-".head
  private val VERTICAL: Char = "|".head
  private val digInstruction = "([UDLR]) (\\w+) \\(#(\\w{6})\\)".r
  private case class DigInstruction(direction: Char, distance: Int, color: String)
  private abstract class TrenchLine(val orientation: Char, val ys: Range, val xs: Range)
  private case class VerticalTrench(x: Int, override val ys: Range) extends TrenchLine(VERTICAL, ys, x to x)
  private case class HorizontalTrench(y: Int, override val xs: Range) extends TrenchLine(HORIZONTAL, y to y, xs)

  private def parseLine(line: String): DigInstruction =
    line match {
      case digInstruction(dir, dis, color) => DigInstruction(dir.head, dis.toInt, color)
    }

  private def useColorCodedInfo(instruction: DigInstruction) =
    val directions = Array('R', 'D', 'L', 'U')
    DigInstruction(directions(instruction.color.last.asDigit), Integer.parseInt(instruction.color.take(5), 16), instruction.color)

  private def digTrench(instructions: List[DigInstruction]): List[TrenchLine] =
    def dig(pos: Pos, instructions: List[DigInstruction]): List[TrenchLine] =
      if instructions.isEmpty then
        List.empty
      else
        val instruction = instructions.head
        val (delta, orientation) = instruction.direction match {
          case 'U' => (up, VERTICAL)
          case 'D' => (down, VERTICAL)
          case 'L' => (left, HORIZONTAL)
          case 'R' => (right, HORIZONTAL)
        }
        val end = (pos._1 + delta._1 * instruction.distance, pos._2 + delta._2 * instruction.distance)
        (orientation match {
          case VERTICAL =>
            val (ys, xs) = if pos._1 < end._1 then (pos._1 to end._1, pos._2 to end._2) else (end._1 to pos._1, end._2 to pos._2)
            VerticalTrench(xs.start, ys)
          case HORIZONTAL =>
            val (ys, xs) = if pos._2 < end._2 then (pos._1 to end._1, pos._2 to end._2) else (end._1 to pos._1, end._2 to pos._2)
            HorizontalTrench(ys.start, xs)
        }) :: dig(end, instructions.tail)
    dig((0, 0), instructions)

  // Vertical first in case horizontal has same x-position.
  private def increasingXPosition(a: TrenchLine, b: TrenchLine) =
    if a.xs.start < b.xs.start then
      true
    else if a.xs.start > b.xs.start then
      false
    else
      a.orientation == VERTICAL

  private def digLagoon(trenches: List[TrenchLine]): Long =
    def isConcave(v1: VerticalTrench, v2: VerticalTrench): Boolean =
      v1.ys.start == v2.ys.start || v1.ys.end == v2.ys.end
    def dig(isDigging: Boolean, trenches: List[TrenchLine]): Long =
      trenches. match {
        case Nil => 0
        case (v1: VerticalTrench) :: (h: HorizontalTrench) :: (v2: VerticalTrench) :: tail =>
          if isConcave(v1, v2) then
            if isDigging then
              h.xs.size - 1 + dig(false, v2 :: tail)
            else
              h.xs.size + dig(false, tail)
          else
            if isDigging then
              h.xs.size + dig(false, tail)
            else
              h.xs.size - 1 + dig(true, v2 :: tail)
        case (v1: VerticalTrench) :: (v2: VerticalTrench) :: (h: HorizontalTrench) :: tail =>
          v2.x - v1.x + dig(true, v2 :: h :: tail)
        case (v1: VerticalTrench) :: (v2: VerticalTrench) :: tail =>
            v2.x - v1.x + 1+ dig(false, tail)
      }
    val minY = trenches.minBy(_.ys.start).ys.start
    val maxY = trenches.maxBy(_.ys.end).ys.end
    (minY to maxY).map(y => dig(false, trenches.filter(t => t.ys.contains(y)).sortWith(increasingXPosition))).sum

  override def exampleAnswerPart1: Long = 62L
  override def solvePart1(lines: List[String]): Long =
    digLagoon(digTrench(lines.map(parseLine)))

  override def exampleAnswerPart2: Long = 952408144115L
  override def solvePart2(lines: List[String]): Long =
    digLagoon(digTrench(lines.map(parseLine).map(useColorCodedInfo)))
}

object Day18 extends App {
  new Day18().solvePuzzles()
}
