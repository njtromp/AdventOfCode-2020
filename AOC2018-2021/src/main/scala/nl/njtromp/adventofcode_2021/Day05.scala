package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day05 extends Puzzle {
  type Pos = (Int, Int)

  private def position(p: String): Pos = (p.split(",")(0).trim.toInt, p.split(",")(1).trim.toInt)

  private def beginEndPoint(line: String): (Pos, Pos) = {
    val p = line.split("->")
    (position(p(0)), position(p(1)))
  }

  private def coveringPositions(p: (Pos, Pos)): List[Pos] = {
    def range(a: Int, b: Int): Range =
      if (a > b) (b to a) else (a to b)
    val b = p._1
    val e = p._2
    if (b._1 == e._1)
      range(b._2, e._2).map((b._1, _)).toList
    else if (b._2 == e._2)
      range(b._1, e._1).map((_, b._2)).toList
    else {
      (b._1 to e._1 by Math.signum(e._1 - b._1).toInt).toList
      .zip((b._2 to e._2 by Math.signum(e._2 - b._2).toInt).toList)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val lineInfo = lines.map(beginEndPoint)
    val vhLines = lineInfo.filter(l => l._1._1 == l._2._1 || l._1._2 == l._2._2)
    val points = vhLines.flatMap(coveringPositions)
    points.groupBy(p => p).count(_._2.size > 1)
  }

  override def solvePart2(lines: List[String]): Long = {
    val lineInfo = lines.map(beginEndPoint)
    val points = lineInfo.flatMap(coveringPositions)
    points.groupBy(p => p).count(_._2.size > 1)
  }
}

object Day05 extends App {
  new Day05().solvePuzzles("/2021/day05.txt")
}
