package nl.njtromp.adventofcode

import scala.collection.mutable

class Day17 extends Puzzle[Long] with SimpleMapTypes {

  private case class State(position: Pos, direction: Delta, straight: Int)

  private def pushCrucible(heatLoss: SimpleMap[Int], start: Pos, finish: Pos, minStraight: Int, maxStraight: Int): Long =
    val heatLosses = mutable.Map.empty[State, Int].withDefaultValue(Int.MaxValue)
    val toBeVisited = mutable.PriorityQueue.empty[State](Ordering.by(s => heatLosses(s))).reverse
    def dijkstra(): Long =
      while toBeVisited.nonEmpty do
        val current = toBeVisited.dequeue()
        if current.position == finish then
          if current.straight >= minStraight then
            return heatLosses(current)
        else
          // Continue in the same directions until the minimum length is reached
          val directions = (if current.position != start && current.straight < minStraight then List(current.direction) else square)
            // Not going back
            .filter(d => !(d._1 == -current.direction._1 && d._2 == -current.direction._2))
            // Can't continue in a straight line for too long
            .filter(d => d != current.direction || current.straight < maxStraight)
            // Must stay within the city
            .filter(d => heatLoss.isOnMap(heatLoss.move(current.position, d)))
          directions.foreach(d =>
            val newPos = heatLoss.move(current.position, d)
            val newState = if d == current.direction then
              State(newPos, d, current.straight + 1)
            else
              State(newPos, d, 1)
            if heatLosses(current) + heatLoss(newPos) < heatLosses(newState) then
              heatLosses(newState) = heatLosses(current) + heatLoss(newPos)
              toBeVisited.enqueue(newState)
          )
      // No solution :-(
      -1

    val starting = State(start, down, 0)
    heatLosses(starting) = 0
    toBeVisited.enqueue(starting)
    dijkstra()

  override def exampleAnswerPart1: Long = 102
  override def solvePart1(lines: List[String]): Long =
    val heatLoss = SimpleMap[Int](lines, _.toCharArray.map(_.asDigit))
    // Start at upper left and finish at lower right corner
    pushCrucible(heatLoss, (0, 0), (heatLoss.height - 1, heatLoss.width - 1), 0, 3)

  override def exampleAnswerPart2: Long = 94
  override def solvePart2(lines: List[String]): Long =
    val heatLoss = SimpleMap[Int](lines, _.toCharArray.map(_.asDigit))
    // Start at upper left and finish at lower right corner
    pushCrucible(heatLoss, (0, 0), (heatLoss.height - 1, heatLoss.width - 1), 4, 10)

}

object Day17 extends App {
  new Day17().solvePuzzles()
}
