package nl.njtromp.adventofcode

import scala.collection.mutable

class Day16 extends Puzzle[Long] with RouteFinding {

  private case class State(p: Pos, d: Delta)

  private def dijkstra(start: Pos, startDir :Delta, finish: Pos, neighbors: Pos => List[Pos]): Long =
    val weightToStart = mutable.Map.empty[State, Long].withDefaultValue(Long.MaxValue)
    val toBeVisited = mutable.PriorityQueue.empty[State](Ordering.by(s => weightToStart(s))).reverse
    val visited = mutable.Set.empty[State]
    val source = mutable.Map.empty[Pos, Pos]

    val startState = State(start, startDir)
    weightToStart += startState -> 0
    toBeVisited.enqueue(startState)

    while toBeVisited.nonEmpty do
      val current = toBeVisited.dequeue()
      if current.p == finish then
        return weightToStart(current)
      // The queue might hold positions with an 'old' (lower) priority so if we encounter one of them, we must skip it.
      if !visited.contains(current) then
        visited += current
        val weight = weightToStart(current)
        neighbors(current.p).foreach(n =>
          val newState = State(n, n - current.p)
          if !visited.contains(newState) then
            // Relax
            val newDir = n - current.p
            val newState = State(n, newDir)
            val delta = 1 + (if current.d == newDir then 0 else 1000)
            if weight + delta < weightToStart(newState) then
              weightToStart(newState) = (weight + delta)
              source(n)  = current.p
            toBeVisited.enqueue(newState)
        )
    Long.MaxValue

  override def exampleAnswerPart1: Long = 7036 + 11048 - 2000
  override def solvePart1(lines: List[String]): Long =
    groupByEmptyLine(lines).map(ls =>
      val map = SimpleMap(ls)
      val start = map.find('S').head
      val finish = map.find('E').head
      def neighbours(p: Pos): List[Pos] =
        map.neighborPositions(p, SQUARE).filter(map(_) != '#')
      val result = dijkstra(start, RIGHT, finish, neighbours)
      result - 1000
    ).sum

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day16 extends App {
  new Day16().solvePuzzles()
}
