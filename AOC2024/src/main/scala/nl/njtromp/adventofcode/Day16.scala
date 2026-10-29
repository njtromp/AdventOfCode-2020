package nl.njtromp.adventofcode

import scala.collection.mutable

class Day16 extends Puzzle[Long] with RouteFinding {

  private case class State(p: Pos, d: Delta)

  private def dijkstra(start: Pos, startDir :Delta, finish: Pos, neighbors: Pos => List[Pos]): (Long, List[Pos]) =
    val weightToStart = mutable.Map.empty[State, Long].withDefaultValue(Long.MaxValue)
    val toBeVisited = mutable.PriorityQueue.empty[State](Ordering.by(s => weightToStart(s))).reverse
    val visited = mutable.Set.empty[State]
    val source = mutable.Map.empty[State, State]

    val startState = State(start, startDir)
    weightToStart += startState -> 0
    toBeVisited.enqueue(startState)

    while toBeVisited.nonEmpty do
      val current = toBeVisited.dequeue()
      if current.p == finish then
        return (weightToStart(current), reconstructPath(startState, current, source).map(_.p))
      // The queue might hold positions with an 'old' (lower) priority so if we encounter one of them, we must skip it.
      if !visited.contains(current) then
        visited += current
        val weight = weightToStart(current)
        neighbors(current.p).foreach(n =>
          val newState = State(n, n - current.p)
          if !visited.contains(newState) then
            // Relax
            val newDir = n - current.p
            val delta = 1 + (if current.d == newDir then 0 else 1000)
            if weight + delta <= weightToStart(newState) then
              weightToStart(newState) = (weight + delta)
              source(newState)  = current
            toBeVisited.enqueue(newState)
        )
    (Long.MaxValue, List.empty)

   private def countBends(path: List[Pos]): Long =
    def removeDups(dirs: List[Delta]): List[Delta] =
      if dirs.isEmpty then
        dirs
      else
        dirs.head :: removeDups(dirs.dropWhile(_ == dirs.head))
    removeDups(path.zip(path.tail).map(_ - _)).size - 1

  private def calculateCosts(path: List[Pos]): Long =
    path.size - 2 + (1000 * countBends(path))

  private def printMap(path: List[Pos], map: SimpleMap[Char]): Unit =
    (0 until map.height).foreach(y =>
      (0 until map.width).foreach(x => print(if path.contains(y, x) then 'O' else map(y, x)))
      println
    )

  override def exampleAnswerPart1: Long = 7036 + 11048
  override def solvePart1(lines: List[String]): Long =
    groupByEmptyLine(lines).map(ls =>
      val map = SimpleMap(ls)
      val start = map.find('S').head
      val finish = map.find('E').head
      def neighbours(p: Pos): List[Pos] =
        map.neighborPositions(p, SQUARE).filter(map(_) != '#')
      val result = dijkstra(start, RIGHT, finish, neighbours)
      calculateCosts((start + LEFT) :: result._2)
    ).sum

  override def exampleAnswerPart2: Long = 45 + 64
  override def solvePart2(lines: List[String]): Long =
    groupByEmptyLine(lines).map(ls =>
      val map = SimpleMap(ls)
      val start = map.find('S').head
      val finish = map.find('E').head
      // The finish can be reached facing any direction, so all states on the finish lead to a single end state.
      val end = State(finish, (0, 0))
      def neighbours(s: State): List[State] =
        if s == end then
          List.empty
        else if s.p == finish then
          List(end)
        else
          map.neighborPositions(s.p, SQUARE).filter(map(_) != '#').map(n => State(n, n - s.p))
      def weight(from: State, to: State): Long =
        if to == end then 0 else 1 + (if from.d == to.d then 0 else 1000)
      allShortestPaths(State(start, RIGHT), end, neighbours, weight).flatten.map(_.p).toSet.size.toLong
    ).sum

}

object Day16 extends App {
  new Day16().solvePuzzles()
}
