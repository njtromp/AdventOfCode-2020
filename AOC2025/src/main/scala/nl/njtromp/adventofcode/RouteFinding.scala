package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.io.AnsiColor._

import nl.njtromp.adventofcode.{SimpleMap, SimpleMapTypes}

trait RouteFinding extends SimpleMapTypes {

  def bfs[A](start: A, finish: A, neighbors: A => List[A]): List[A] =
    val source = mutable.Map.empty[A, A]
    val toBeVisited = mutable.Queue.empty[A]
    val visited = mutable.Set.empty[A]
    toBeVisited.enqueue(start)
    while (toBeVisited.nonEmpty)
      val current = toBeVisited.dequeue()
      if (current == finish)
        return reconstructPath(start, finish, source)
      if (!visited.contains(current))
        visited += current
        neighbors(current).foreach(n => 
          if (!visited.contains(n))
            toBeVisited.enqueue(n)
            source(n) = current
        )
    Nil

  def bfs[A](map: SimpleMap[A], canReach: (A, A) => Boolean, start: Pos, finish: Pos): List[Pos] =
    val source = mutable.Map.empty[Pos, Pos]
    val toBeVisited = mutable.Queue.empty[Pos]
    val visited = mutable.Set.empty[Pos]
    toBeVisited.enqueue(start)
    while (toBeVisited.nonEmpty)
      val current = toBeVisited.dequeue()
      if (current == finish)
        return reconstructPath(start, finish, source)
      if (!visited.contains(current))
        visited += current
        val neighbors = map.neighborPositions(current, SQUARE).filter(n => canReach(map(current), map(n)))
        neighbors.foreach(n =>
          if (!visited.contains(n))
            toBeVisited.enqueue(n)
            source(n) = current
        )
    Nil

  def dijkstra[A](start: A, finish: A, neighbors: A => List[A], weight: (A, A) => Long): List[A] =
    val weightToStart = mutable.Map.empty[A, Long].withDefaultValue(Long.MaxValue)
    val source = mutable.Map.empty[A, A]
    val toBeVisited = mutable.PriorityQueue.empty[A](using Ordering.by(n => -weightToStart(n)))
    val visited = mutable.Set.empty[A]
    weightToStart(start) = 0
    toBeVisited.enqueue(start)
    while toBeVisited.nonEmpty do
      val current = toBeVisited.dequeue()
      if current == finish then
        return reconstructPath(start, finish, source)
      // The queue might hold positions with an 'old' (lower) priority so if we encounter one of them, we must skip it.
      if !visited.contains(current) then
        visited += current
        val length = weightToStart(current)
        neighbors(current).foreach(n =>
          // Relax
          val delta = weight(current, n)
          if length + delta < weightToStart(n) then
            weightToStart(n) = (length + delta)
            source += n -> current
          if !visited.contains(n) then
            toBeVisited.enqueue(n)
        )
    List.empty

  def dijkstra[A](map: SimpleMap[A], canReach: (A, A) => Boolean, start: Pos, finish: Pos): List[Pos] =
    val weightToStart = mutable.Map.empty[Pos, Int].withDefaultValue(Int.MaxValue)
    val source = mutable.Map.empty[Pos, Pos]
    val toBeVisited = mutable.PriorityQueue.empty[Pos](Ordering.by(p => -weightToStart(p)))
    val visited = mutable.Set.empty[Pos]
    weightToStart += start -> 0
    toBeVisited.enqueue(start)
    while toBeVisited.nonEmpty do
      val current = toBeVisited.dequeue()
      if current == finish then
        return reconstructPath(start, finish, source)
      // The queue might hold positions with an 'old' (lower) priority so if we encounter one of them, we must skip it.
      if !visited.contains(current) then
        visited += current
        val length = weightToStart(current)
        val neighbors = map.neighborPositions(current, SQUARE).filter(n => canReach(map(current), map(n)))
        neighbors.foreach(n =>
          // Relax
          if length + 1 < weightToStart(n) then
            weightToStart(n) = length + 1
            source(n) = current
          if !visited.contains(n) then
            toBeVisited.enqueue(n)
        )
    List.empty

  protected def reconstructPath[A](start: A, current: A, source: mutable.Map[A, A]): List[A] =
    def reconstructPath(current: A): List[A] =
      if (current == start)
        List(start)
      else
        reconstructPath(source(current)) ++ List(current)
    reconstructPath(current)

  protected def printColoredPath[A](map: SimpleMap[A], path: List[Pos], colorMapping: A => Int): Unit =
    println
    val ps = path.toSet
    (0 until map.height).foreach(y =>
      (0 until map.width).foreach(x =>
        val p = (y, x)
        if (ps.contains(p))
          print(s"\u001B[48;5;${colorMapping(map(p))}m$BLACK${map(p)}$RESET")
        else
          print(s"\u001B[38;5;${colorMapping(map(p))}m${map(p)}$RESET")
      )
      println
    )

  protected def printPath[A](map: SimpleMap[A], path: List[Pos]): Unit =
    def decodeMove(move: List[Pos]): Char =
        val delta: Delta = (move.last._1 - move.head._1, move.last._2 - move.head._2)
        if (delta == UP)
          '^'
        else if (delta == DOWN)
          'v'
        else if (delta == LEFT)
          '<'
        else
          '>'
    println
    val ps = path.sliding(2).toList
    (0 until map.height).foreach(y =>
      (0 until map.width).foreach(x =>
        val moves = ps.filter(_.head == (y, x))
        if (moves.nonEmpty)
          print(decodeMove(moves.head))
        else
          print(map(y, x))
      )
      println
    )

}

