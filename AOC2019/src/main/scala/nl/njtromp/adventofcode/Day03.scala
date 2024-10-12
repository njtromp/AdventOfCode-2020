package nl.njtromp.adventofcode

class Day03 extends Puzzle[Long] {
  type Pos = (Int, Int)

  private case class Line(d: Char, horizontal: Range, vertical: Range) {
    def isHorizontal: Boolean = horizontal.size > 1
    def isVertical: Boolean = vertical.size > 1
    def intersects(other: Line): Boolean =
      if isHorizontal then
        other.isVertical && horizontal.contains(other.horizontal.start) && other.vertical.contains(vertical.start)
      else
        other.isHorizontal && other.horizontal.contains(horizontal.start) && vertical.contains(other.vertical.start)
    def intersection(other: Line): Pos =
      if isHorizontal then
        (other.horizontal.start, vertical.start)
      else
        (horizontal.start, other.vertical.start)
    def numberOfSteps: Int = if isHorizontal then horizontal.size - 1 else vertical.size - 1
    def stepsTill(x: Pos): Int =
      if isHorizontal then
        if d == 'R' then x._1 - horizontal.start else horizontal.end - x._1
      else
        if d == 'U' then x._2 - vertical.start else vertical.end - x._2
  }

  private def parseLine(line: String): List[Line] =
    // We need to ensure that 'start <= end', and make them inclusive on the fly
    def fix(r: Range): Range = if r.start <= r.end then r.inclusive else Range(r.end, r.start).inclusive
    line.split(",").foldLeft(((0, 0), List.empty[Line]))((a, l) =>
      val ((x, y), ls) = a
      l.head match
        case 'U' => ((x, y + l.drop(1).toInt), Line('U', fix(Range(x, x)), fix(Range(y, y + l.drop(1).toInt))) :: ls)
        case 'D' => ((x, y - l.drop(1).toInt), Line('D', fix(Range(x, x)), fix(Range(y, y - l.drop(1).toInt))) :: ls)
        case 'R' => ((x + l.drop(1).toInt, y), Line('R', fix(Range(x, x + l.drop(1).toInt)), fix(Range(y, y))) :: ls)
        case 'L' => ((x - l.drop(1).toInt, y), Line('L', fix(Range(x, x - l.drop(1).toInt)), fix(Range(y, y))) :: ls)
    )._2.reverse

  private def findClosestCrossing(wires: List[List[Line]]): Long =
    wires.head.flatMap(w1 => wires.last
      .filter(_.intersects(w1)) // Find all intersecting line segments
      .map((w1, _)) // and keep them together
    )
      .map(x => x._1.intersection(x._2)) // Convert to the crossing position
      .filter(_ != (0, 0)) // Lines don't cross at the origin
      .map(p => Math.abs(p._1) + Math.abs(p._2)) // Manhattan distance to the origin
      .min

  private def findLowestNumberOfSteps(wires: List[List[Line]]): Long =
    wires.head.flatMap(w1 => wires.last
        .filter(_.intersects(w1)) // Find all intersecting line segments
        .map((w1, _)) // and keep them together
      )
      .map(x => (x._1.intersection(x._2), x)) // The two crossing line segments and the position of the crossing
      .filter(_._1 != (0, 0)) // Lines don't cross at the origin
      .map(x =>
        // The number of steps for the first wire up till the crossing
        wires.head.takeWhile(_ != x._2._1).map(_.numberOfSteps).sum + x._2._1.stepsTill(x._1)
        // and the number of steps for the second wire
        + wires.last.takeWhile(_ != x._2._2).map(_.numberOfSteps).sum + x._2._2.stepsTill(x._1)
    )
      .min

  override def exampleAnswerPart1: Long = 6 + 159 + 135
  override def solvePart1(lines: List[String]): Long =
    lines.map(parseLine)
      .grouped(2)
      .map(findClosestCrossing)
      .sum

  override def exampleAnswerPart2: Long = 30 + 610 + 410
  override def solvePart2(lines: List[String]): Long =
    lines.map(parseLine)
      .grouped(2)
      .map(findLowestNumberOfSteps)
      .sum

}

object Day03 extends App {
  new Day03().solvePuzzles()
}
