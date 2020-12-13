package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day13 extends App {
  val lines: List[String] = Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle13.txt")).getLines().toList

  def solvePart1(lines: List[String]):Int = {
    val time = lines(0).toInt
    val bus = lines(1).split(",").filter(id => id != "x").map(id => (id.toInt, id.toInt - time % id.toInt)).sortBy(i => i._2).toList.head
    bus._1 * bus._2
  }

  def solvePart2(lines: List[String]): Long = {
    val busses = lines(1).split(",").zipWithIndex.filter(b => b._1 != "x").map(b => (b._1.toInt, b._2))
    var t: Long = 0
    while (t >= 0 && !busses.tail.forall(b => b._1 - (t % b._1) == b._2)) {
      t += busses.head._1
    }
    t
  }

  println(s"Answer part 1: ${solvePart1(lines)}")
  println(s"Answer part 2: ${solvePart2(lines)}")
}
