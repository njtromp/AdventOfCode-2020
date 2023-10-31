package nl.njtromp.adventofcode

import scala.collection.mutable
import io.AnsiColor._

import nl.njtromp.adventofcode.{SimpleMap, SimpleMapTypes}

object Dijkstra extends SimpleMapTypes {

  def findRoute[A](map: SimpleMap[A], canReach: (A, A) => Boolean, start: Pos, finish: Pos): List[Pos] =
    val distanceToStart = mutable.Map.empty[Pos, Int].withDefaultValue(Int.MaxValue)
    val source = mutable.Map.empty[Pos, Pos]
    val toBeVisited = mutable.PriorityQueue.empty[Pos](Ordering.by(p => -distanceToStart(p)))
    val visited = mutable.Set.empty[Pos]

    def constructPath(current: Pos): List[Pos] =
      if (current == start)
        List(start)
      else
        constructPath(source(current)) ++ List(current)

    distanceToStart += start -> 0
    toBeVisited.enqueue(start)
    while (toBeVisited.nonEmpty)
      val current = toBeVisited.dequeue()
      if (current == finish)
        return constructPath(finish)
      // The queue might hold positions with an 'old' (lower) priority so if we encounter one of them, we must skip it.
      if (!visited.contains(current))
        visited += current
        val length = distanceToStart(current)
        val neighbors = map.neighborPositions(current, square).filter(n => canReach(map(current), map(n)))
        neighbors.foreach(n => {
          if (!visited.contains(n))
            toBeVisited.enqueue(n)
          // Relax
          if (length + 1 < distanceToStart(n))
            distanceToStart += n -> (length + 1)
            source += n -> current
        })
    Nil

  def printPath[A](map: SimpleMap[A], path: List[Pos], colorMapping: A => Int): Unit =
    def decodeMove(move: List[List[Pos]]): Char =
      move match {
        case m :: Nil => val delta = (m.last._1 - m.head._1, m.last._2 - m.head._2)
          delta match {
            case up => '^'
            case down => 'v'
            case left => '<'
            case right => '>'
          }
      }
    println
    val ps = path.toSet
    (0 until map.height).foreach(y =>
      (0 until map.width).foreach(x =>
        val p = (y, x)
        if (ps.contains(p))
          print(s"\u001B[48;5;${colorMapping(map(p))}m${BLACK}${map(p)}${RESET}")
        else
          print(s"\u001B[38;5;${colorMapping(map(p))}m${map(p)}${RESET}")
      )
      println
    )

}

