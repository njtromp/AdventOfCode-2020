package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day01 extends App {
  var numbers: Set[Int] = Set()
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle01.txt")).getLines) {
    numbers += line.toInt
  }
  val answerPart1 = numbers.filter(n => numbers.contains(2020 - n)).product
  println(s"Answer part 1: $answerPart1");

}
