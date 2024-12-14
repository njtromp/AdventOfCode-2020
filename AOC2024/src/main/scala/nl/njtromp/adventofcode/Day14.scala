package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day14 extends Puzzle[Long] {

  type Pos = (Long, Long)
  type Delta = (Long, Long)

  extension (p: Pos)
    def +(d: Delta): Pos = (p._1 + d._1, p._2 + d._2)
    def limit: Pos = ((p._1 + width) % width, (p._2 + height) % height)

  private var width = 11
  private var height = 7

  private case class Robot(pos: Pos, delta: Delta) {
    def move: Robot =
      Robot((pos + delta).limit, delta)
    def isNeighbour(other: Robot): Boolean =
      Math.abs(pos._1 - other.pos._1) <= 1 &&
        Math.abs(pos._2 - other.pos._2) <= 1
  }

  private def parseRobot(line: String): Robot =
    line match
      case s"p=$x,$y v=$dx,$dy" => Robot((x.toLong, y.toLong), (dx.toLong, dy.toLong))

  @tailrec
  private def move(robots: List[Robot], time: Long): List[Robot] =
    if time == 0 then
      robots
    else
      move(robots.map(_.move), time - 1)

  private def safetyFactor(robots: List[Robot]): Long =
    val middleX = width / 2
    val middleY = height / 2
    val upperLeft = robots.count(r => r.pos._1 < middleX && r.pos._2 < middleY)
    val upperRight = robots.count(r => r.pos._1 < middleX && r.pos._2 > middleY)
    val lowerLeft = robots.count(r => r.pos._1 > middleX && r.pos._2 < middleY)
    val lowerRight = robots.count(r => r.pos._1 > middleX && r.pos._2 > middleY)
    upperLeft * upperRight * lowerLeft * lowerRight

  @tailrec
  private def findEasterEgg(robots: List[Robot], time: Long): Long =
    def allHaveNeighbour(robots: List[Robot]): Boolean =
      val neighbourCount = robots.count(r => robots.exists(o => o != r && o.isNeighbour(r)))
      if neighbourCount == 367 then
        printEasterEgg(robots)
        true
      else
        false
    if allHaveNeighbour(robots) then
      time
    else
      findEasterEgg(robots.map(_.move), time + 1)

  private def printEasterEgg(robots: List[Robot]): Unit =
    val poss = robots.map(_.pos).toSet
    (0 to height).foreach(y =>
      (0 to width).foreach(x => print(if poss.contains(x, y) then '#' else '.'))
      println
    )
    println

  override def exampleAnswerPart1: Long = 12
  override def solvePart1(lines: List[String]): Long =
    val robots = lines.map(parseRobot)
    val result = safetyFactor(move(robots, 100))
    width = 101
    height = 103
    result

  private var simulatePart1 = true
  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if simulatePart1 then
      simulatePart1 = false
      0
    else
      val robots = lines.map(parseRobot)
      findEasterEgg(robots, 0)

}

object Day14 extends App {
  new Day14().solvePuzzles()
}
