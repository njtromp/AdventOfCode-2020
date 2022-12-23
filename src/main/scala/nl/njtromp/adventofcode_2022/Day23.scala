package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day23 extends Puzzle2 {
  type Pos = (Long, Long)
  val NORTH: Int = 0
  val SOUTH: Int = 1
  val WEST: Int = 2
  val EAST: Int = 3
  val NORTH_SCAN: Array[Pos] = Array((-1, -1), (-1, 0), (-1, 1))
  val SOUTH_SCAN: Array[Pos] = Array((1, -1), (1, 0), (1, 1))
  val WEST_SCAN: Array[Pos] = Array((-1, -1), (0, -1), (1, -1))
  val EAST_SCAN: Array[Pos] = Array((-1, 1), (0, 1), (1, 1))
  val SCANS: Array[Array[Pos]] = Array(NORTH_SCAN, SOUTH_SCAN, WEST_SCAN, EAST_SCAN)
  val RADAR: Set[Pos] = Set((-1, -1), (-1, 0), (-1, 1),
                            (0, -1),           (0, 1),
                            (1, -1),  (1, 0),  (1, 1))

  class Elve(val index: Int, var y: Long, var x: Long) {
    var scanId: Int = 0
    var direction: Int = scanId
    def canMove(elves: Set[Pos]): Boolean = RADAR.map(r => (y + r._1, x + r._2)).intersect(elves).nonEmpty
    def proposeMove(elves: Set[Pos]): Pos = {
      (0 until 4).foreach(s => {
        direction = (s + scanId) % 4
        if (SCANS(direction).map(d => (y + d._1, x + d._2)).toSet.intersect(elves).isEmpty) {
          return direction match {
            case NORTH => (y - 1, x)
            case SOUTH => (y + 1, x)
            case WEST => (y, x - 1)
            case EAST => (y, x + 1)
          }
        }
      })
      direction = -1
      (y, x)
    }
    def move(): Unit = {
      direction match {
        case NORTH => y -= 1
        case SOUTH => y += 1
        case WEST => x -= 1
        case EAST => x += 1
        case _ =>
      }
    }
    def nextScan(): Unit = scanId = (scanId + 1) % 4
  }

  @tailrec
  final def moveElves(round: Long, elves: Array[Elve]): Unit = {
    if (round > 0) {
      val currentLocations = elves.map(e => (e.y, e.x)).toSet
      val proposingElves = elves.filter(_.canMove(currentLocations)).map(_.index)
      val proposedLocations = proposingElves.map(i => elves(i).proposeMove(currentLocations))
      val movingElves = proposingElves.zip(proposedLocations).filter(e => proposedLocations.count(_ == e._2) == 1).map(_._1)
      movingElves.foreach(elves(_).move())
      elves.foreach(_.nextScan())
      moveElves(round - 1, elves)
    }
  }

  @tailrec
  final def moveElvesUntilStop(round: Long, elves: Array[Elve]): Long = {
    val currentLocations = elves.map(e => (e.y, e.x)).toSet
    val proposingElves = elves.filter(_.canMove(currentLocations)).map(_.index)
    if (proposingElves.length == 0) return round
    val proposedLocations = proposingElves.map(i => elves(i).proposeMove(currentLocations))
    val movingElves = proposingElves.zip(proposedLocations).filter(e => proposedLocations.count(_ == e._2) == 1).map(_._1)
    movingElves.foreach(elves(_).move())
    elves.foreach(_.nextScan())
    moveElvesUntilStop(round + 1, elves)
  }

  override def exampleAnswerPart1: Long = 110L
  override def solvePart1(lines: List[String]): Long = {
    var index = -1
    val elves = lines.zipWithIndex
      .flatMap(l => l._1.zipWithIndex.filter(_._1 == '#').map(e => {
        index += 1
        new Elve(index, l._2, e._2)
      })).toArray
    moveElves(10L, elves)
    (elves.maxBy(_.y).y - elves.minBy(_.y).y + 1L) * (elves.maxBy(_.x).x - elves.minBy(_.x).x + 1L) - elves.length
  }

  override def exampleAnswerPart2: Long = 20
  override def solvePart2(lines: List[String]): Long = {
    var index = -1
    val elves = lines.zipWithIndex
      .flatMap(l => l._1.zipWithIndex.filter(_._1 == '#').map(e => {
        index += 1
        new Elve(index, l._2, e._2)
      })).toArray
    moveElvesUntilStop(1L, elves)
  }

}

object Day23 extends App{
  new Day23().solvePuzzles("/2022/day23.txt")
}
