package nl.njtromp.adventofcode

class Day15 extends Puzzle[Long] {
  type Pos = (Long, Long)
  private val sensorReading = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
  private case class SensorReading(sX: Long, sY: Long, bX: Long, bY: Long) {
    val range: Long = distance(sX - bX, sY - bY)
    def distance(deltaX: Long, deltaY: Long): Long = Math.abs(deltaX) + Math.abs(deltaY)
    def rangeAt(y: Long): Option[LongRange] =
      val deltaY = Math.abs(y - sY)
      if (deltaY <= range)
        Some(LongRange(sX - (range - deltaY), sX + (range - deltaY)))
      else
        None
  }

  private def combine(ranges: List[LongRange]): List[LongRange] = ranges match {
    case Nil => Nil
    case r :: Nil => r :: Nil
    case a :: b :: tail =>
      val combined = a.combine(b)
      if (combined.size == 1)
        combine(combined.head :: tail)
      else
        a :: combine(b :: tail)
  }

  private var y = 10L
  override def exampleAnswerPart1: Long = 26
  override def solvePart1(lines: List[String]): Long =
    val readings = lines.map({case sensorReading(sX, sY, bX, bY) => SensorReading(sX.toLong, sY.toLong, bX.toLong, bY.toLong)})
    val beacons  = readings.filter(_.bY == y).map(_.bY).toSet
    val sortedRanges = combine(readings.map(_.rangeAt(y)).filter(_.nonEmpty).map(_.get).sortBy(_.start))
    y = 2000000L
    sortedRanges.map(_.size).sum - beacons.size

  private var max = 20
  override def exampleAnswerPart2: Long = 56000011
  override def solvePart2(lines: List[String]): Long =
//    max = 4000000
    -1
}

object Day15 extends App{
  new Day15().solvePuzzles("/day15.txt")
}
