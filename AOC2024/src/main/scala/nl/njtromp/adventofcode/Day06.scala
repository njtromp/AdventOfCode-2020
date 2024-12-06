package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day06 extends Puzzle[Long] with SimpleMapTypes {

  private def turnRight(dir: Delta): Delta =
    dir match
      case UP => RIGHT
      case RIGHT => DOWN
      case DOWN => LEFT
      case LEFT => UP

  private def followGuard(guard: Pos, dir: Delta, map: SimpleMap[Char]): mutable.Set[Pos] =
    val path = mutable.Set(guard)
    @tailrec
    def follow(guard: Pos, dir: Delta): mutable.Set[Pos] =
      val moved = map.move(guard, dir)
      if map.isOnMap(moved) then
        if map(moved) != '#' then
          path += moved
          follow(moved, dir)
        else
          follow(guard, turnRight(dir))
      else
        path
    follow(guard, dir)

  private def containsLoop(guard: Pos, dir: Delta, map: SimpleMap[Char]): Boolean =
    val path = mutable.Set((guard, UP))
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
    follow(guard, dir)

  override def exampleAnswerPart1: Long = 41
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toArray)
    val guard = map.find('^').head
    followGuard(guard, UP, map).size

  override def exampleAnswerPart2: Long = 6
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toArray)
    val guard = map.find('^').head
    map.find('.')
      .count(dot =>
        map(dot) = '#'
        val hasLoop = containsLoop(guard, UP, map)
        map(dot) = '.'
        hasLoop
      )

}

object Day06 extends App {
  new Day06().solvePuzzles()
}
