package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec
import scala.collection.mutable

class Day18 extends Puzzle2 {
  type Cube = (Int, Int, Int)
  private def distance(c1: Cube, c2: Cube): Int = Math.abs(c1._1 - c2._1) + Math.abs(c1._2 - c2._2) + Math.abs(c1._3 - c2._3)

  private def findNeighbours(cubes: List[Cube]): Int = {
    cubes match {
      case Nil => 0
      case c :: tail => tail.count(distance(_, c) == 1) + findNeighbours(tail)
    }
  }

  private def getNeighbours(cube: (Int, Int, Int)): Set[Cube] = {
    Set(
      (cube._1 + 1, cube._2, cube._3),
      (cube._1 - 1, cube._2, cube._3),
      (cube._1, cube._2 + 1, cube._3),
      (cube._1, cube._2 - 1, cube._3),
      (cube._1, cube._2, cube._3 + 1),
      (cube._1, cube._2, cube._3 - 1),
    )
  }

  private def findAir(shape: Set[Cube]): Set[Cube] = {
    val xLimits = shape.minBy(_._1)._1 - 1 to shape.maxBy(_._1)._1 + 1
    val yLimits = shape.minBy(_._2)._2 - 1 to shape.maxBy(_._2)._2 + 1
    val zLimits = shape.minBy(_._3)._3 - 1 to shape.maxBy(_._3)._3 + 1
    def limit(locs: Set[Cube]): Set[Cube] = locs.filter(l => xLimits.contains(l._1) && yLimits.contains(l._2) && zLimits.contains(l._3))
    @tailrec
    def floodFill(locs: Set[Cube], air: mutable.Set[Cube]): mutable.Set[Cube] = {
      if (locs.isEmpty) {
        air
      } else {
        val airNeighbours = limit(getNeighbours(locs.head)).diff(air).diff(shape)
        air ++= airNeighbours
        floodFill(locs.tail ++ airNeighbours, air)
      }
    }
    val air = mutable.Set((0, 0,0 ))
    floodFill(Set((0, 0, 0)), air).toSet
  }

  override def exampleAnswerPart1: Long = 64
  override def solvePart1(lines: List[String]): Long = {
    val cubes = lines.map(_.split(",")).map(c => (c(0).toInt, c(1).toInt, c(2).toInt))
    val neighbours = findNeighbours(cubes)
    cubes.size * 6 - neighbours * 2
  }

  override def exampleAnswerPart2: Long = 58
  override def solvePart2(lines: List[String]): Long = {
    val cubes = lines.map(_.split(",")).map(c => (c(0).toInt, c(1).toInt, c(2).toInt)).toSet
    val air = findAir(cubes)
    val contacts = cubes.toList.map(c => air.count(a => distance(c, a) == 1))
    contacts.sum
  }

}

object Day18 extends App{
  new Day18().solvePuzzles("/2022/day18.txt")
}
