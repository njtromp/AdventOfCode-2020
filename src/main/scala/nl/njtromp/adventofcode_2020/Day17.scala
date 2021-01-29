package nl.njtromp.adventofcode_2020

import scala.annotation.tailrec

class Day17 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    val startCells = lines.zipWithIndex
      .flatMap(li => li._1.zipWithIndex.map(ci => if (ci._1 == '#') Some(li._2, ci._2, 0) else None))
      .filter(_.isDefined)
      .map(_.get)
      .toSet
    cycle(startCells, 6).size
  }

  def solvePart2(lines: List[String]): Long = {
    val startCells = lines.zipWithIndex
      .flatMap(li => li._1.zipWithIndex.map(ci => if (ci._1 == '#') Some(li._2, ci._2, 0, 0) else None))
      .filter(_.isDefined)
      .map(_.get)
      .toSet
    cycle4D(startCells, 6).size
  }

  @tailrec
  private def cycle(activeCells: Set[(Int, Int, Int)], cycles: Int): Set[(Int, Int, Int)] = {
    if (cycles == 0) activeCells else
    cycle(
        activeCells.filter(c => (2 to 3).contains((neighbors(c) intersect activeCells).size))
      union
        activeCells.flatMap(c => (neighbors(c) diff activeCells).filter(i => (activeCells intersect neighbors(i)).size == 3))
      , cycles - 1)
  }

  def neighbors(pos: (Int, Int, Int)): Set[(Int, Int, Int)] = {
    (-1 to 1).flatMap(x => (-1 to 1).flatMap(y => (-1 to 1).filter(z => !(x == 0 && y == 0 && z == 0))map(z => (pos._1 + x, pos._2 + y, pos._3 + z)))).toSet
  }

  @tailrec
  private def cycle4D(activeCells: Set[(Int, Int, Int, Int)], cycles: Int): Set[(Int, Int, Int, Int)] = {
    if (cycles == 0) activeCells else
      cycle4D(
        activeCells.filter(c => (2 to 3).contains((neighbors(c) intersect activeCells).size))
          union
          activeCells.flatMap(c => (neighbors(c) diff activeCells).filter(i => (activeCells intersect neighbors(i)).size == 3))
        , cycles - 1)
  }

  def neighbors(pos: (Int, Int, Int, Int)): Set[(Int, Int, Int, Int)] = {
    (-1 to 1).flatMap(x => (-1 to 1).flatMap(y => (-1 to 1).flatMap(z => (-1 to 1).filter(w => !(x == 0 && y == 0 && z == 0 && w == 0))map(w => (pos._1 + x, pos._2 + y, pos._3 + z, pos._4 + w))))).toSet
  }
}

object Day17 extends App {
  new Day17().solvePuzzles("/2020/input-puzzle17.txt")
}
