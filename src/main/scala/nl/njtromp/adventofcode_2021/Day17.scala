package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day17 extends Puzzle {
  type Pos = (Int, Int) // (X, Y)
  type Velocity = (Int, Int) // (X direction, Y direction)
  type TargetRange = (Range, Range)

  private val TargetArea = "target area: x=(\\d+)..(\\d+), y=-(\\d+)..-(\\d+)".r // (X-range, Y-range)

  def isOnTarget(p: Pos, t: TargetRange): Boolean = t._1.contains(p._1) && t._2.contains(p._2)
  def hasOverShoot(p: Pos, t: TargetRange): Boolean = p._1 > t._1.max || p._2 < t._2.min

  def drag(x: Int): Int = if (x > 0) x - 1 else if (x < 0) x + 1 else 0
  def oneStep(p: Pos, v: Velocity): (Pos, Velocity) = ((p._1 + v._1, p._2 + v._2), (drag(v._1), v._2 - 1))

  def shootProbe(v: Velocity, t: TargetRange): List[Pos] = {
    def shootProbe(p: Pos, v: Velocity): List[Pos] = {
      val (pos, velocity) = oneStep(p, v)
      if (isOnTarget(pos, t))
        List(pos)
      else {
        if (!hasOverShoot(pos, t))
        pos :: shootProbe(pos, velocity)
      else
        List.empty
      }
    }
    val trajectory = shootProbe((0, 0), v)
    if (trajectory.nonEmpty && isOnTarget(trajectory.reverse.head, t)) trajectory else List.empty
  }

  override def solvePart1(lines: List[String]): Long = {
    val targetArea = lines.map({
      case TargetArea(minX, maxX, minY, maxY) => (minX.toInt to maxX.toInt, -minY.toInt to -maxY.toInt)
    }).head
    val trajectories = (1 to 100).flatMap(x => (-50  to 500).map(y => shootProbe((x, y), targetArea)))
    trajectories.filterNot(_.isEmpty).map(_.maxBy(_._2)._2).max
  }

  override def solvePart2(lines: List[String]): Long = {
    val targetArea = lines.map({
      case TargetArea(minX, maxX, minY, maxY) => (minX.toInt to maxX.toInt, -minY.toInt to -maxY.toInt)
    }).head
    val trajectories = (1 to 200).flatMap(x => (-1000 to 1000).map(y => shootProbe((x, y), targetArea)))
    trajectories.count(_.nonEmpty)
  }

}

object Day17 extends App {
  new Day17().solvePuzzles("/2021/day17.txt")
}
