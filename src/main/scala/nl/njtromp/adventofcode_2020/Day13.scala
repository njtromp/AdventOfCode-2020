package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day13 extends App {
  val ns: Iterator[String] = Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle13.txt")).getLines()

  def solvePart1(ns: Iterator[String]):Int = {
    val asList = ns.toList
    val time = asList(0).toInt
    val busses = asList(1).split(",").filter(id => id != "x").map(id => (id.toInt, id.toInt - time % id.toInt)).sortBy(i => i._2).toList
    val bus = busses.head
    bus._1 * bus._2
  }

  def solvePart2(ns: Iterator[String]): Int = -1

  println(s"Answer part 1: ${solvePart1(ns)}")
  println(s"Answer part 2: ${solvePart2(ns)}")
}
