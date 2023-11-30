package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.{Puzzle2, LongRange}


class Day15 extends Puzzle2 {
  type Pos = (Long, Long)
  private val SensorBeacon = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

  case class Sensor(pos: Pos, beacon: Pos) {
    val reach: Long = distanceTo(beacon)
    def distanceTo(p: Pos): Long = hamilton(pos, p)
    def coveringRange(y: Long): LongRange = {
      val distanceToY: Long = hamilton(pos, (y, pos._2))
      val start: Long = pos._2 - (reach - distanceToY)
      val last: Long = pos._2 + (reach - distanceToY)
      LongRange(start, last)
    }
  }

  private def hamilton(a: Pos, b: Pos): Long = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  private def readSensorData(lines: List[String]): List[Sensor] =
    lines.map({
      case SensorBeacon(sx, sy, bx, by) => Sensor((sy.toLong, sx.toLong), (by.toLong, bx.toLong))
    })

  private def combineRanges(ranges: List[LongRange]): List[LongRange] = {
    if (ranges.size == 1) {
      ranges
    } else {
      val base = ranges.head
      val overlapping = ranges.tail.filter(_.isOverlapping(base))
      if (overlapping.isEmpty) {
        base :: combineRanges(ranges.tail)
      } else {
        val remaining = ranges.tail.filterNot(overlapping.contains(_))
        val combined = overlapping.foldLeft(base)((a, b) => a.combine(b).head)
        combineRanges(combined :: remaining)
      }
    }
  }

  override def exampleAnswerPart1: Long = 26
  var y = 10L
  override def solvePart1(lines: List[String]): Long = {
    val sensors = readSensorData(lines)
    val sensorRanges = combineRanges(sensors.filter(s => s.distanceTo((y, s.pos._2)) <= s.reach).map(_.coveringRange(y)))
    val sensorsOnLine = sensors.filter(_.pos._1 == y).map(_.pos._2).toSet.count(s => sensorRanges.exists(_.contains(s)))
    val beaconsOnLine = sensors.filter(_.beacon._1 == y).map(_.beacon._2).toSet.count(s => sensorRanges.exists(_.contains(s)))
    y = 2000000L // Prepare for the real thing ;-)
    sensorRanges.map(_.size).sum - sensorsOnLine - beaconsOnLine
  }

  def findDistressLocation(sensors: List[Sensor]): Pos = {
    val candidateRanges = (0L to 4000000L).filter(y => {
      val ranges = sensors.filter(s => s.distanceTo((y, s.pos._2)) <= s.reach).map(_.coveringRange(y))
      if (ranges.isEmpty) false else combineRanges(ranges).size == 2
    }).map(y => (y, combineRanges(sensors.filter(s => s.distanceTo((y, s.pos._2)) <= s.reach).map(_.coveringRange(y)))))
    // The next two lines are a shortcut because in my case the first range precedes the second range, so we can use
    // that knowledge in out advantage, just use the last value of the first range.
    val targetRanges = candidateRanges.filter(p => p._2.head.last + 2 == p._2.last.first)
    (targetRanges.head._1, targetRanges.head._2.head.last + 1L)
  }

  var maxX: Long = 20L
  var maxY: Long = 20L
  override def exampleAnswerPart2: Long = 56000011L
  override def solvePart2(lines: List[String]): Long = {
    val sensors = readSensorData(lines).sortBy(s => hamilton((0,0), s.pos))
    val pos = findDistressLocation(sensors)
    maxX = 4000000L // Prepare for the real thing ;-)
    maxY = 4000000L // Prepare for the real thing ;-)
    pos._1 + pos._2 * 4000000L
  }

}

object Day15 extends App{
  new Day15().solvePuzzles("/2022/day15.txt")
}
