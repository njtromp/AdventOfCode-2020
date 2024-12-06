package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day06 extends Puzzle[Long] with SimpleMapTypes {

  private def turnRight(d: Delta): Delta =
    d match
      case UP => RIGHT
      case RIGHT => DOWN
      case DOWN => LEFT
      case LEFT => UP

  private def followGuard(guard: Pos, dir: Delta, map: SimpleMap[Char], path: mutable.Set[Pos]): mutable.Set[Pos] =
    @tailrec
    def follow(guard: Pos, dir: Delta): Unit =
      val moved = map.move(guard, dir)
      if map.isOnMap(moved) then
        if map(moved) != '#' then
          path += moved
          follow(moved, dir)
        else
          follow(guard, turnRight(dir))
    path += guard
    follow(guard, dir)
    path

  private def makesLoop(guard: Pos, dir: Delta, map: SimpleMap[Char]): Boolean =
    val path = mutable.Set.empty[(Pos, Delta)]
    @tailrec
    def follow(guard: Pos, dir: Delta): Boolean =
      val moved = map.move(guard, dir)
      if !map.isOnMap(moved) then
        false
      else if path.contains((moved, dir)) then
        true
      else if map(moved) != '#' then
        path += ((moved, dir))
        follow(moved, dir)
      else
        follow(guard, turnRight(dir))
    path += ((guard, UP))
    follow(guard, dir)

  override def exampleAnswerPart1: Long = 41
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toArray)
    val guard = map.find('^').head
    followGuard(guard, UP, map, mutable.Set.empty).size

  override def exampleAnswerPart2: Long = 6
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toArray)
    val guard = map.find('^').head
    val spaces = map.find('.')
    spaces.count(c =>
      map(c) = '#'
      val isLoop = makesLoop(guard, UP, map)
      map(c) = '.'
      isLoop
    )

}

object Day06 extends App {
  new Day06().solvePuzzles()
}
