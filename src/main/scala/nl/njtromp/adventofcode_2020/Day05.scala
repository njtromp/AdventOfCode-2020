package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day05 extends App {
  private var seatNumbers: Set[Int] = Set.empty
  for (line <- Source.fromInputStream(getClass.getResourceAsStream("/2020/input-puzzle05.txt")).getLines) {
    seatNumbers += Integer.parseInt(line.replaceAll("[FL]", "0").replaceAll("[BR]", "1"), 2)
  }
  println(s"Answer part 1: ${seatNumbers.max}")
  println(s"Answer part 2: ${(8 to 127 * 8).filter(n => !(seatNumbers contains n) && (seatNumbers contains (n - 1)) && (seatNumbers contains (n + 1))).head}")
}
