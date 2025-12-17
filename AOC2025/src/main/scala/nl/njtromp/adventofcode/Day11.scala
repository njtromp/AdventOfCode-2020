package nl.njtromp.adventofcode

import scala.collection.mutable

class Day11 extends Puzzle[Long] with RouteFinding {
  private val YOU = "you"
  private val OUT = "out"
  private val SVR = "svr"
  private val DAC = "dac"
  private val FFT = "fft"

  private case class Device(name: String, outputs: List[String])

  private def findPaths(start: String, finish: String, devices: List[Device]): Long =
    val deviceMap = devices.map(d => (d.name, d)).toMap
    val nrOfPaths = mutable.Map.empty[String, Long]
    def findPaths(current: String): Long =
      if current == finish then
        1
      else if nrOfPaths.contains(current) then
        nrOfPaths(current)
      else
        val paths = deviceMap(current).outputs.map(d => findPaths(d)).sum
        nrOfPaths(current) = paths
        paths
    findPaths(start)

  override def exampleAnswerPart1: Long = 5
  override def solvePart1(lines: List[String]): Long =
    val devices = lines.splitOnEmptyLines.head.map(l => Device(l.substring(0, 3), l.split(' ').tail.toList))
    findPaths(YOU, OUT, devices)

  override def exampleAnswerPart2: Long = 2
  override def solvePart2(lines: List[String]): Long =
    val devices = Device(OUT, List.empty) :: lines.splitOnEmptyLines.last.map(l => Device(l.substring(0, 3), l.split(' ').tail.toList))
    // There is no path from DAC to FFT, so the path can be split into three sections :-)
    findPaths(SVR, FFT, devices) * findPaths(FFT, DAC, devices) * findPaths(DAC, OUT, devices)

}

object Day11 extends App {
  new Day11().solvePuzzles()
}
