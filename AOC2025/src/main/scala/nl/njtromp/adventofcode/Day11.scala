package nl.njtromp.adventofcode

import scala.collection.mutable

class Day11 extends Puzzle[Long] {
  private val YOU = "you"
  private val OUT = "out"
  private val SVR = "svr"
  private val DAC = "dac"
  private val FFT = "fft"

  private case class Device(name: String, outputs: List[String])

  override def exampleAnswerPart1: Long = 5
  override def solvePart1(lines: List[String]): Long =
    val devices = lines.splitOnEmptyLines.head.map(l => Device(l.substring(0, 3), l.split(' ').tail.toList))
    val deviceMap = devices.map(d => (d.name, d)).toMap
    val connections = mutable.Map[String, Long]().empty.withDefaultValue(0L)
    connections(YOU) = 1
    val needsVisiting = mutable.Queue[String]().empty
    needsVisiting.enqueue(YOU)
    while needsVisiting.nonEmpty do
      val device: String = needsVisiting.dequeue()
      if device != OUT then
        deviceMap(device).outputs.foreach(o =>
          connections(o) += 1
          needsVisiting.enqueue(o)
      )
    connections(OUT)

  override def exampleAnswerPart2: Long = 2
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day11 extends App {
  new Day11().solvePuzzles()
}
