package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day01 extends App {
  val ns = Source.fromInputStream(Day01.getClass.getResourceAsStream("/2020/input-puzzle01.txt")).getLines().map(n => n.toInt).toSet

  def solvePart1(ns: Set[Int]):Int =ns.filter(n => ns.contains(2020 - n)).product

  def solvePart2(ns: Set[Int]): Int = ns.filter(n => ns.exists(m => ns.contains(2020 - n - m))).product

  println(s"Answer part 1: ${solvePart1(ns)}")
  println(s"Answer part 2: ${solvePart2(ns)}")
}
