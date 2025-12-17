package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day07 extends Puzzle[Long] with SimpleMapTypes {
  private val START = 'S'
  private val SPLITTER = '^'

  private def countSplits(map: SimpleMap[Char], beams: List[Pos]): Long =
    @tailrec
    def followBeams(beams: Set[Pos], nrOfSplits: Long): Long =
      val oneDown = beams.map(_ + DOWN)
      if map.isOnMap(oneDown.head) then
        val splitters = oneDown.filter(map(_) == SPLITTER)
        val straightDown = oneDown -- splitters
        val splittedBeams = splitters.flatMap(s => Set(s + LEFT, s + RIGHT))
        val newBeams = straightDown ++ splittedBeams
        followBeams(newBeams, nrOfSplits + splitters.size)
      else
        nrOfSplits
    followBeams(beams.toSet, 0L)

  private def countParallelSplits(map: SimpleMap[Char], particle: Pos): Long =
    val nrOfPaths = mutable.Map.empty[Pos, Long]
    def followParticle(particle: Pos): Long =
      if !map.isOnMap(particle) then
        1
      else if nrOfPaths.contains(particle) then
        nrOfPaths(particle)
      else if map(particle) == SPLITTER then
        val paths = followParticle(particle + LEFT) + followParticle(particle + RIGHT)
        nrOfPaths(particle) = paths
        paths
      else
        followParticle(particle + DOWN)
    followParticle(particle)

  override def exampleAnswerPart1: Long = 21
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines)
    val start = map.find(START)
    countSplits(map, start)

  override def exampleAnswerPart2: Long = 40
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines)
    val start = map.find(START).head
    countParallelSplits(map, start)

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
