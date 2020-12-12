package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day01 extends App {
  val ns = Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle01.txt")).getLines().map(n => n.toInt).toSet
  println(s"Answer part 1: ${ns.filter(n => ns.contains(2020 - n)).product}");
  println(s"Answer part 2: ${ns.filter(n => ns.exists(m => ns.contains(2020 - n - m))).product}");
}
