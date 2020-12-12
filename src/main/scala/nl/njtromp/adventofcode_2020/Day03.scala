package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day03 extends App {
  var numberOfTrees: Long = 0
  var column = 0
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle03.txt")).getLines) {
    numberOfTrees += (if (line.charAt(column) == '#') 1 else 0)
    column = (column + 3) % line.length
  }
  println(s"Answer part 1: $numberOfTrees")

  var totalTreeCount: Long = 1
  val slopes = Array(Array(1,1), Array(3, 1), Array(5, 1), Array(7, 1), Array(1, 2))
  for (slope <- slopes) {
    column = 0
    numberOfTrees = 0
    var lineNr = 0
    for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle03.txt")).getLines) {
      if (lineNr % slope(1) == 0) {
        numberOfTrees += (if (line.charAt(column) == '#') 1 else 0)
        column = (column + slope(0)) % line.length
      }
      lineNr += 1
    }
    totalTreeCount *= numberOfTrees
  }
  println(s"Answer part 2: $totalTreeCount")

}
