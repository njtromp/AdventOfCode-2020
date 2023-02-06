package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.StringPuzzle

class Day11 extends StringPuzzle {
  private val GRID_SIZE = 300

  private def createFuelcells(serialNumber: Int): Array[Array[Int]] = {
    def powerLevel(x: Int, y: Int): Int = {
      val rackId = x + 10
      val powerLevel = (rackId * y + serialNumber) * rackId
      (powerLevel % 1000) / 100 - 5
    }
    val fuelCells = Array.ofDim[Int](GRID_SIZE, GRID_SIZE)
    (1 to GRID_SIZE).foreach(y =>
      (1 to GRID_SIZE).foreach(x => {
        fuelCells(y - 1)(x - 1) = powerLevel(x, y)
      })
    )
    fuelCells
  }

  private def makeGrid(fuelCells: Array[Array[Int]], squareSize: Int): List[(Int, Int, Int)] = {
    (1 to GRID_SIZE - squareSize).flatMap(y =>
      (1 to GRID_SIZE - squareSize).map(x =>
        (x, y, fuelCells.slice(y - 1, y - 1 + squareSize).map(_.slice(x - 1, x - 1 + squareSize).sum).sum)
      )
    ).toList
  }

  override def exampleAnswerPart1: String = "33,45"
  override def solvePart1(lines: List[String]): String = {
    val fuelCells = createFuelcells(lines.head.toInt)
    val grids = makeGrid(fuelCells, 3)
    val maxSquare = grids.maxBy(_._3)
    s"${maxSquare._1},${maxSquare._2}"
  }

  override def exampleAnswerPart2: String = "90,269,16"
  override def solvePart2(lines: List[String]): String = {
    val fuelCells = createFuelcells(lines.head.toInt)
    val maxSquare = (1 to 17).map(size => {
      val grids = makeGrid(fuelCells, size)
      val maxSquare = grids.maxBy(_._3)
      (maxSquare._1, maxSquare._2, size, maxSquare._3)
    }).maxBy(_._4)
    s"${maxSquare._1},${maxSquare._2},${maxSquare._3}"
  }

}

object Day11 extends App {
  new Day11().solvePuzzles("/2018/day11.txt")
}
