package nl.njtromp.adventofcode

import scala.collection.mutable

class Day17 extends Puzzle[Long] with SimpleMapTypes {
  private val maxStraight = 3

  private case class State(pos: Pos, dir: Delta, length: Int)

  private def pushCrucible(heatLoss: SimpleMap[Int], start: Pos, finish: Pos): Long =
    val heatLosses = mutable.Map.empty[State, Int].withDefaultValue(Int.MaxValue)
    val toBeVisited = mutable.PriorityQueue.empty[State](Ordering.by(s => heatLosses(s))).reverse
    def pushCrucible(): Long =
      while toBeVisited.nonEmpty do
        val current = toBeVisited.dequeue()
        if current.pos == finish then
          return heatLosses(current)
        else
          val directions = square
            // Not going back
            .filter(d => !(d._1 == -current.dir._1 && d._2 == -current.dir._2))
            // Can't continue in a straight line for too long
            .filter(d => d != current.dir || current.length < maxStraight)
            // Must stay within the city
            .filter(d => heatLoss.isOnMap(heatLoss.move(current.pos, d)))
          directions.foreach(d =>
            val newPos = heatLoss.move(current.pos, d)
            val newState = if d == current.dir then
              State(newPos, d, current.length + 1)
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
    pushCrucible()

  override def exampleAnswerPart1: Long = 102
  override def solvePart1(lines: List[String]): Long =
    val heatLoss = SimpleMap[Int](lines, _.toCharArray.map(_.asDigit))
    // Start at upper left and finish at lower right corner
    pushCrucible(heatLoss, (0, 0), (heatLoss.height - 1, heatLoss.width - 1))

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day17 extends App {
  new Day17().solvePuzzles()
}
