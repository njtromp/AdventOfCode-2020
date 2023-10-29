package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import io.AnsiColor._

import nl.njtromp.adventofcode.{SimpleMap, SimpleMapTypes}

object Dijkstra extends SimpleMapTypes {

  def findRoute[A](map: SimpleMap[A], canReach: (A, A) => Boolean, priority: Pos => Int, start: Pos, finish: Pos): List[Pos] =
    var bestLength: Int = Int.MaxValue
    val numberOfSteps = Array.fill(map.height, map.width)(Int.MaxValue)
    val source = mutable.Map[Pos, Pos]()
    val visited = mutable.Set[Pos]()
    val toBeVisited = mutable.ArrayBuffer[Pos]()

    def constructPath(): List[Pos] =
      val path = mutable.Stack.empty[Pos]
      var current = finish
      while (current != start)
        path.push(current)
        current = source(current)
      path.push(start)
      path.toList

    @tailrec
    def dijkstra(): Unit =
      if (toBeVisited.nonEmpty)
        val current = toBeVisited.minBy(priority)
        toBeVisited -= current
        visited += current
        val length = numberOfSteps(current._1)(current._2)
        if (current == finish) {
          if (length < bestLength)
            bestLength = length
        } else {
          val neighbors = map.neighborPositions(current, square)
            .filter(p => canReach(map(current), map(p)))
          neighbors.foreach(n => {
            if (length + 1 < numberOfSteps(n._1)(n._2))
              numberOfSteps(n._1)(n._2) = length + 1
              source += n -> current
            if (!visited.contains(n) && !toBeVisited.contains(n))
              toBeVisited += n
          })
          dijkstra()
        }
    numberOfSteps(start._1)(start._2) = 0
    toBeVisited += start
    dijkstra()
    constructPath()

  def printPath[A](map: SimpleMap[A], path: List[Pos]): Unit =
    def decodeMove(move: List[List[Pos]]): Char =
      move match {
        case m :: Nil => val delta: Delta = (m.last._1 - m.head._1, m.last._2 - m.head._2)
          if (delta == up)
            '^'
          else if (delta == down)
            'v'
          else if (delta == right)
            '>'
          else
            '<'
      }
    println
    val ps = path.toSet
    (0 until map.height).foreach(y =>
      (0 until map.width).foreach(x =>
        val p = (y, x)
        if (ps.contains(p)) {
          val s = s"${BOLD}${WHITE}${BLACK_B}${map(p)}${RESET}"
          print(s)
        }
        else
          print(map(p))
      )
      println
    )

}

