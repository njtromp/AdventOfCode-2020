package nl.njtromp.adventofcode

class Day15 extends Puzzle[Long] {
  type Pos = (Long, Long)
  private val sensorReading = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
  private case class SensorReading(sX: Long, sY: Long, bX: Long, bY: Long) {
    val range: Long = distance(sX - bX, sY - bY)
    def distance(deltaX: Long, deltaY: Long): Long = Math.abs(deltaX) + Math.abs(deltaY)
    def beaconFree(y: Long): List[Long] =
      if (Math.abs(y - sY) > range)
        Nil
      else
        (sX - range to sX + range).filter(x => distance(x - sX, y - sY) <= range).toList
  }

  private var y = 10L
  override def exampleAnswerPart1: Long = 26
  override def solvePart1(lines: List[String]): Long =
    val readings = lines.map({case sensorReading(sX, sY, bX, bY) => SensorReading(sX.toLong, sY.toLong, bX.toLong, bY.toLong)})
    val beacons  = readings.filter(_.bY == y).map(_.bY)
    val beaconFree = (readings.flatMap(_.beaconFree(y)).toSet -- beacons.toSet).size
    y = 2000000L
    beaconFree

  private var max = 20
  override def exampleAnswerPart2: Long = 56000011
  override def solvePart2(lines: List[String]): Long =
//    max = 4000000
    -1
}

object Day15 extends App{
  new Day15().solvePuzzles("/day15.txt")
}
