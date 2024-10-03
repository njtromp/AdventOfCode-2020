package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.io.AnsiColor.*

class Day17 extends Puzzle[Long] with SimpleMapTypes {
  private val maxStraight = 3

  private def pushCrucible(map: SimpleMap[Int], maxStraight: Int, start: Pos, finish: Pos): List[Pos] =
    val directions = List(down, right, up, left)
    val heatLosses = Array.fill[Long](map.height, map.width)(Long.MaxValue)
    val previous = mutable.Map.empty[Pos, Pos]
    val visited = mutable.Set.empty[Pos]
    val toBeVisited = mutable.PriorityQueue.empty[Pos](Ordering.by(p => heatLosses(p._1)(p._2))).reverse
    def delta(p1: Pos, p2: Pos): Delta = (p1._1 - p2._1, p1._2 - p2._2)
    def toStraight(path: List[Pos]): Boolean =
      val moves = path.zip(path.tail).map(delta)
      moves.groupBy(n => n).map(_._2.size).max >= maxStraight
    def reconstructPath(current: Pos): List[Pos] =
      if current == start then
        List(start)
      else
        current :: reconstructPath(previous(current))
    def printHeatLoss(): Unit =
      (0 until map.height).foreach(y =>
        (0 until map.width).foreach(x =>
          if heatLosses(y)(x) < Int.MaxValue then
            print(f"${heatLosses(y)(x)}%5d")
          else
            print("   . ")
        )
        println(s"    ${map.row(y)}")
      )
      println
    def pushCrucible(): Unit =
      while toBeVisited.nonEmpty do
        val current = toBeVisited.dequeue()
        if current == finish then
          return
        else if !visited.contains(current) then
          printHeatLoss()
          visited.add(current)
          val path = reconstructPath(current).take(maxStraight)
          val neighbors = map.neighborPositions(current, directions)
            .filter(n => !visited.contains(n))
            .filter(n => !toStraight(n :: path))
          neighbors.foreach(n =>
            if heatLosses(current._1)(current._2) + map(n) < heatLosses(n._1)(n._2) then
              heatLosses(n._1)(n._2) = heatLosses(current._1)(current._2) + map(n)
              previous.put(n, current)
              toBeVisited.enqueue(n)
          )
      // No solution :-(
      List.empty[Pos]

    toBeVisited.enqueue(start)
    heatLosses(start._1)(start._2) = 0
    pushCrucible()
    reconstructPath(finish).reverse

//  println(s"\b\b\b\b\b$REVERSED$heatLoss$RESET")
//  print(f"\b\b\b\b\b$heatLoss%5d")

  def printPath(map: SimpleMap[Int], path: Set[Pos]): Unit =
    (0 until map.height).foreach(y =>
      (0 until map.width).foreach(x =>
        if path.contains((y, x)) then
          print('*')
        else
          print(map(y, x))
      )
      println
    )

  override def exampleAnswerPart1: Long = 102
  override def solvePart1(lines: List[String]): Long =
    val heatLoss = SimpleMap[Int](lines, _.toCharArray.map(_.asDigit))
    val path = pushCrucible(heatLoss, maxStraight, (0, 0), (heatLoss.height - 1, heatLoss.width - 1))
    if path.nonEmpty then
      printPath(heatLoss, path.toSet)
      // Don't include the heat loss at the start
      path.tail.map(p => heatLoss(p)).sum
    else
      -1

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day17 extends App {
  println("Please run with: -Xss512M")
  println("84056 is too high")
  println("84042 is too high")
  println("84027 is too high")
  println("1089 is not correct")
  new Day17().solvePuzzles()
}
