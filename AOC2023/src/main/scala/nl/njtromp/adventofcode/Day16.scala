package nl.njtromp.adventofcode

import scala.collection.mutable

class Day16 extends Puzzle[Long] with SimpleMapTypes {
  private val nextDirections = Map(
    (down, '.') -> List(down),
    (down, '|') -> List(down),
    (down, '-') -> List(left, right),
    (down, '/') -> List(left),
    (down, '\\') -> List(right),

    (up, '.') -> List(up),
    (up, '|') -> List(up),
    (up, '-') -> List(left, right),
    (up, '/') -> List(right),
    (up, '\\') -> List(left),

    (left, '.') -> List(left),
    (left, '|') -> List(up, down),
    (left, '-') -> List(left),
    (left, '/') -> List(down),
    (left, '\\') -> List(up),

    (right, '.') -> List(right),
    (right, '|') -> List(up, down),
    (right, '-') -> List(right),
    (right, '/') -> List(up),
    (right, '\\') -> List(down)
  )

  private def followBeams(map: SimpleMap[Char], current: Pos, direction: Delta, path: List[Pos]):  List[List[Pos]] =
    val visited = mutable.Set.empty[(Pos, Delta)]
    def followBeam(current: Pos, direction: Delta, path: List[Pos]): List[List[Pos]] =
      if visited.contains((current, direction)) then
        // Just to be sure...
        List(path)
      else
        // Register that we arrived here in combination with the direction
        visited += ((current, direction))
        val directions = nextDirections(direction, map(current))
        directions.flatMap(direction =>
          val nextPos = map.move(current, direction)
          if map.isOnMap(nextPos) then
            followBeam(nextPos, direction, current :: path)
          else
            List(current :: path)
        )
    followBeam(current, direction, path)

  override def exampleAnswerPart1: Long = 46
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap[Char](lines, _.toCharArray)
    val paths = followBeams(map, (0,0), right, List.empty)
    paths.flatten.toSet.size

  override def exampleAnswerPart2: Long = 51
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap[Char](lines, _.toCharArray)
    val starts = (0 until map.height).flatMap(r => List(((r, 0), right), ((r, map.width - 1), left))) ++
      (0 until map.width).flatMap(c => List(((0, c), down), ((map.height - 1, c), up)))
    starts.foldLeft(0L)((a, s) => Math.max(a, followBeams(map, s._1, s._2, List.empty).flatten.toSet.size))
}

object Day16 extends App {
  println("Please run with: -Xss512M")
  new Day16().solvePuzzles()
}
