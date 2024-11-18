package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.boundary, scala.util.boundary.break

class Day24 extends Puzzle[Long] {
  private var testArea: (Double, Double) = (7, 27)

  private case class HailStone(val x0: Double, val y0: Double, val z0: Double, val dx: Double, val dy: Double, val dz: Double) {
    // The gradient of the path in the X-Y plane can be expressed as dy / dx
    private lazy val gradient = dy / dx
    def intersectionPoint2D(other: HailStone): (Double, Double) =
      // Using the gradient, this leads to the following equation for a line with only x and y
      // y = y0 + g0.(x - x0)
      // Two lines intersect when their y value are equal, leading to
      // y0 + g0.(x - x0) = y1 + g1.(x - x1)
      // For the x-value this leads to
      // y0 + g0.x - g0.x0 = y1 + g1.x - g1.x1
      // x.(g0 - g1) = y1 - y0  + g0.x0 - g1.x1
      // x = (y1 - y0  + g0.x0 - g1.x1) / (g0 - g1)
      val x = (other.y0 - y0 + gradient * x0 - other.gradient * other.x0) / (gradient - other.gradient)
      // And a y-coordinate using the first equation (see line 11)
      (x, y0 + gradient * (x - x0))
    def rotate: HailStone =
      this.copy(x0 = y0, y0 = z0, z0 = x0, dx = dy, dy = dz, dz = dx)
    override def toString: String = f"$x0%1.0f, $y0%1.0f, $z0%1.0f @ $dx%1.0f, $dy%1.0f, $dz%1.0f"
  }

  private def parseLine(line: String): HailStone =
    val parts = line.replaceAll(" ", "").replaceAll("@", ",").split(',')
    HailStone(parts(0).toDouble, parts(1).toDouble, parts(2).toDouble, parts(3).toDouble, parts(4).toDouble, parts(5).toDouble)

  private def countCollidingXY(hailStones: List[HailStone]): Long =
    if hailStones.isEmpty then
      0
    else
      def findIntersectionsWithinTestArea(pairs: List[(HailStone, HailStone)]): Long =
        pairs.map(p =>
          val (x, y) = p._1.intersectionPoint2D(p._2)
          val t1 = (x - p._1.x0) / p._1.dx
          val t2 = (x - p._2.x0) / p._2.dx
          val inTheFuture = t1 > 0 && t2 > 0
          val withingTestArea = x >= testArea._1 && x <= testArea._2 &&
            y >= testArea._1 && y <= testArea._2
          if inTheFuture && withingTestArea then
            1
          else
            0
        ).sum
      val hailStone = hailStones.head
      findIntersectionsWithinTestArea(hailStones.tail.map((hailStone, _))) + countCollidingXY(hailStones.tail)

  private def intersectionPoints(hailStones: List[HailStone]): mutable.Map[(Double, Double), Long] =
    val intersections = mutable.Map.empty[(Double, Double), Long].withDefaultValue(0L)
    @tailrec
    def intersectionsPoints(hailStone: HailStone, others: List[HailStone]): Unit =
      if others.nonEmpty then
        others.foreach(h => intersections(h.intersectionPoint2D(hailStone)) += 1L)
        intersectionsPoints(others.head, others.tail)
    intersectionsPoints(hailStones.head, hailStones.tail)
    intersections

  private def findIntersection(hailStones: List[Day24.this.HailStone]): (Long, Long) =
    boundary:
      val range = 600 // The maximum velocity is 505 so 600 should be enough to cover it all
      (-range to range).foreach(dx =>
        (-range to range).foreach(dy =>
          val intersections = intersectionPoints(hailStones.map(h => h.copy(dx = h.dx - dx, dy = h.dy - dy)))
          val intersection = intersections.filter(_._2 > 3)
          if intersection.nonEmpty then
            break((intersection.head._1._1.toLong, intersection.head._1._2.toLong))
        )
      )
      (0, 0)

  override def exampleAnswerPart1: Long = 2
  override def solvePart1(lines: List[String]): Long =
    val hailStones = lines.map(parseLine)
    val withinTestArea = countCollidingXY(hailStones)
    testArea = (200000000000000L, 400000000000000L)
    withinTestArea

  override def exampleAnswerPart2: Long = 47
  override def solvePart2(lines: List[String]): Long =
    // With some information from: https://www.youtube.com/watch?v=nP2ahZs40U8
    testArea = (Double.NegativeInfinity, Double.PositiveInfinity)
    val hailStones = lines.map(parseLine).take(10)
    val (x, y) = findIntersection(hailStones)
    val (_, z) = findIntersection(hailStones.map(_.rotate))
    x + y + z
}

object Day24 extends App {
  new Day24().solvePuzzles()
}
