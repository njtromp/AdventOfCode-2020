package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day09 extends Puzzle[Long] {
  private type Pos = (Int, Int)
  private type Square = Line
  private val UP = 'U'
  private val DOWN = 'D'

  extension (r: Range)
    private def overlaps(o: Range): Boolean = r.contains(o.start) || r.contains(o.end)

  private case class Line(horizontal: Range, vertical: Range) {
    val isHorizontal: Boolean = horizontal.size > 1
    val isVertical: Boolean = vertical.size > 1
    val area: Int = horizontal.size * vertical.size
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
  }
  private object Line {
    def apply(p1: Pos, p2: Pos): Line =
      new Line(
        Range(Math.min(p1._1, p2._1), Math.max(p1._1, p2._1)).inclusive,
        Range(Math.min(p1._2, p2._2), Math.max(p1._2, p2._2)).inclusive
      )
  }

  private def keepInnerSquares(squares: List[Square], polygon: List[Line]): List[Square] =
    val (minX, maxX, minY, maxY) = squares.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue))((a, p) =>
      (Math.min(a._1, p.horizontal.start - 1), Math.max(a._2, p.horizontal.end + 1), Math.min(a._3, p.vertical.start), Math.max(a._4, p.vertical.end))
    )
    @tailrec
    def filter(y: Int, squares: List[Square]): List[Square] =
      def outsideParts(polygonParts: List[Line]): List[Line] =
        @tailrec
        def scan(start: Int, x: Int, isInside: Boolean, outsideParts: List[Line]): List[Line] =
          if x >= maxX then
            if !isInside then Line((start, y), (x, y)) :: outsideParts else outsideParts
          else
            val linesAtX = polygonParts.filter(_.horizontal.contains(x))
            linesAtX.size match
              case 0 =>
                scan(start, x + 1, isInside, outsideParts)
              case 1 =>
                if isInside then
                  scan(x + 1, x + 1, false, outsideParts)
                else
                  scan(x, x + 1, true, Line((start, y), (x - 1, y)) :: outsideParts)
              case 2 =>
                val (newX, firstVerticalLine) = if linesAtX.head.isVertical then
                  (linesAtX.last.horizontal.end, linesAtX.head)
                else
                  (linesAtX.head.horizontal.end, linesAtX.last)
                val firstDirection = if firstVerticalLine.vertical.start == y then DOWN else UP
                val linesAtNewX = polygonParts.filter(_.horizontal.contains(newX))
                val secondVerticalLine = if linesAtNewX.head.isVertical then
                  linesAtNewX.head
                else
                  linesAtNewX.last
                val secondDirection = if secondVerticalLine.vertical.start == y then DOWN else UP
                val switchSides = firstDirection != secondDirection
                if isInside then
                  if switchSides then
                    scan(newX + 1, newX + 1, false, outsideParts)
                  else
                    scan(newX + 1, newX + 1, true, outsideParts)
                else
                  if switchSides then
                    scan(newX + 1, newX + 1, true, Line((start, y), (x - 1, y)) :: outsideParts)
                  else
                    scan(newX + 1, newX + 1, false, Line((start, y), (x - 1, y)) :: outsideParts)
        scan(minX, minX, false, List.empty)
      if y == maxY then
        squares
      else
        val squaresAtY = squares.filter(_.vertical.contains(y))
        val polygonsAtY = polygon.filter(_.vertical.contains(y))
        val outsideRanges: List[Line] = outsideParts(polygonsAtY)
        val invalidSquares = squaresAtY.filter(s => outsideRanges.exists(p => p.horizontal.overlaps(s.horizontal)))
        filter(y + 1, squares.filterNot(invalidSquares.contains))
    filter(minY, squares)

  override def exampleAnswerPart1: Long = 50
  override def solvePart1(lines: List[String]): Long =
    val redTiles = lines.map {case s"$x,$y" => (x.toLong, y.toLong)}
    redTiles.combinations(2).map(t =>
      val dX = Math.abs(t.head._1 - t.last._1) + 1
      val dY = Math.abs(t.head._2 - t.last._2) + 1
      dX * dY
    ).max

  override def exampleAnswerPart2: Long = 24
  override def solvePart2(lines: List[String]): Long =
    val redTiles: List[Pos] = lines.map {case s"$x,$y" => (x.toInt, y.toInt)}
    val ranges = redTiles.zip(redTiles.tail :+ redTiles.head)
    val polygon = ranges.map((p1, p2) => Line(p1, p2))
    // For some reason the type alias Square can not be used here :-(
    val squares = redTiles.combinations(2).map(ps => Line(ps.head, ps.last)).toList
    val innerSquares = keepInnerSquares(squares, polygon)
    innerSquares.map(_.area).max

}

object Day09 extends App {
  new Day09().solvePuzzles()
}
