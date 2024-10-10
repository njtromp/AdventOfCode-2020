package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day25 extends Puzzle {

  private type Pos = (Int, Int)
  private def locateCucumbers(lines: List[String]): (Set[Pos], Set[Pos]) = {
    lines.filter(_.nonEmpty).zipWithIndex.foldLeft((Set.empty[Pos], Set.empty[Pos]))((acc, l) => {
      val east = l._1.zipWithIndex.filter(_._1 == '>').map(c =>(c._2, l._2))
      val south = l._1.zipWithIndex.filter(_._1 == 'v').map(c =>(c._2, l._2))
      (acc._1 ++ east, acc._2 ++ south)
    })
  }

  @tailrec
  private def moveCucumbers(steps: Int, east: Set[(Int, Int)], south: Set[(Int, Int)], width: Int, heigth: Int): Int = {
    def moveEast(c: Pos): Pos = ((c._1 + 1) % width, c._2)
    def moveSouth(c: Pos): Pos = (c._1, (c._2 + 1) % heigth)
    def printCucumbers(east: Set[(Int, Int)], south: Set[(Int, Int)]): Unit = {
      for (y <- 0 until heigth) {
        for (x <- 0 until width)
          print(if (east.contains(x, y)) ">" else if (south.contains(x, y)) "v" else ".")
        println
      }
      println
    }
//    printCucumbers(east, south)
    val canMoveEast = east.filterNot(c => east.contains(moveEast(c)) || south.contains(moveEast(c)))
    val stationaryEast = east -- canMoveEast
    val movedEast = canMoveEast.map(moveEast)
    val newEast = stationaryEast ++ movedEast

    val canMoveSouth = south.filterNot(c => newEast.contains(moveSouth(c)) || south.contains(moveSouth(c)))
    val stationarySouth = south -- canMoveSouth
    val movedSouth = canMoveSouth.map(moveSouth)
    val newSouth = stationarySouth ++ movedSouth
    if (newEast == east && newSouth == south)
      steps
    else
      moveCucumbers(steps + 1, newEast, newSouth, width, heigth)
  }

  override def solvePart1(lines: List[String]): Long = {
    val (eastMoving, southMoving) = locateCucumbers(lines)
    moveCucumbers(1, eastMoving, southMoving, lines.head.length, lines.count(_.nonEmpty))
  }

  override def solvePart2(lines: List[String]): Long = 0 // There is no part 2
}

object Day25 extends App {
  new Day25().solvePuzzles("/2021/day25.txt")
}
