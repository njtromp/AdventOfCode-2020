package nl.njtromp.adventofcode

class Day09 extends Puzzle[Long] {
  private type Pos = (Int, Int)
  private val UP = 'U'
  private val DOWN = 'D'

  private case class Line(horizontal: Range, vertical: Range) {
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
  }

  private def fillInner(mapping: Array[Array[Boolean]], redTiles: List[(Int, Int)], minX: Int, maxX: Int, minY: Int, maxY: Int): Unit =
    val ranges = redTiles.zip(redTiles.tail :+ redTiles.head)
    val polygon = ranges.map((p1, p2) =>
      Line(
        Range(Math.min(p1._1, p2._1), Math.max(p1._1, p2._1)).inclusive,
        Range(Math.min(p1._2, p2._2), Math.max(p1._2, p2._2)).inclusive
      ))
    val linesAtY = (minY to maxY).map(y => (y, polygon.filter(_.vertical.contains(y)))).toMap
    (minY to maxY).foreach(y =>
      val lines = linesAtY(y)
      var startX = maxX
      var endX = minX
      lines.foreach(l =>
        if l.isVertical then
          mapping(y - minY)(l._1.start - minX) = true
        else
          (l._1.start to l._1.end).foreach(x => mapping(y - minY)(x - minX) = true)
        startX = Math.min(startX, l.horizontal.start)
        endX = Math.max(endX, l.horizontal.end)
      )
      var inSide = false
      var x = startX
      while x < endX do
        var linesAtX = lines.filter(_.horizontal.contains(x))
        linesAtX.size match
          case 0 =>
            mapping(y - minY)(x - minX) = inSide
          case 1 =>
            inSide = !inSide
            mapping(y - minY)(x - minX) = inSide
          case 2 =>
            val firstVerticalLine = if linesAtX.head.isVertical then
              x = linesAtX.last.horizontal.end
              linesAtX.head
            else
              x = linesAtX.head.horizontal.end
              linesAtX.last
            val firstDirection = if firstVerticalLine.vertical.start == y then DOWN else UP
            linesAtX = lines.filter(_.horizontal.contains(x))
            val secondVerticalLine = if linesAtX.head.isVertical then
              linesAtX.head
            else
              linesAtX.last
            val secondDirection = if secondVerticalLine.vertical.start == y then DOWN else UP
            if firstDirection != secondDirection then inSide = !inSide
        x += 1
    )

  override def exampleAnswerPart1: Long = 50
  override def solvePart1(lines: List[String]): Long =
    val redTiles = lines.map {case s"$x,$y" => (x.toLong, y.toLong)}
    redTiles.combinations(2).map(t =>
      val dX = Math.abs(t.head._1 - t.last._1) + 1
      val dY = Math.abs(t.head._2 - t.last._2) + 1
      dX * dY
    ).max

  private def allRedAndGreen(map: Array[Array[Boolean]], c1: Pos, c2: Pos): Boolean =
    !(Math.min(c1._2, c2._2) to Math.max(c1._2, c2._2)).exists(y =>
      (Math.min(c1._1, c2._1) to Math.max(c1._1, c2._1)).exists(x  => !map(y)(x))
    )


  override def exampleAnswerPart2: Long = 24
  override def solvePart2(lines: List[String]): Long =
    val redTiles: List[Pos] = lines.map {case s"$x,$y" => (x.toInt, y.toInt)}
    val (minX, maxX, minY, maxY) = redTiles.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue))((a, p) =>
      (Math.min(a._1, p._1), Math.max(a._2, p._1), Math.min(a._3, p._2), Math.max(a._4, p._2))
    )
    val mapping = Array.ofDim[Boolean](maxY - minY + 1, maxX - minX + 1)
    fillInner(mapping, redTiles, minX, maxX, minY, maxY)
    redTiles.combinations(2)
      .filter(t => allRedAndGreen(mapping, (t.head._1 - minX, t.head._2 - minY), (t.last._1 - minX, t.last._2 - minY)))
      .map(t =>
        val dX = Math.abs(t.head._1 - t.last._1) + 1
        val dY = Math.abs(t.head._2 - t.last._2) + 1
        dX * dY
      ).max

}

object Day09 extends App {
  new Day09().solvePuzzles()
}
