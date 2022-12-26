package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day17 extends Puzzle2 {
  type Pos = (Long, Long)
  abstract class Rock(val shape: Set[Pos])
  class Minus extends Rock(Set((0, 0), (0, 1), (0, 2), (0, 3)))
  class Plus extends Rock(Set((2, 1),
                      (1, 0), (1, 1), (1, 2),
                              (0, 1)))
  class Hook extends Rock(Set((2, 2),
                              (1, 2),
              (0, 0), (0, 1), (0, 2)))
  class Pipe extends Rock(Set((3, 0),
                              (2, 0),
                              (1, 0),
                              (0, 0)))
  class Dot extends Rock(Set((1, 0), (1, 1),
                             (0, 0), (0, 1)))
  private val rocks: Array[Rock] = Array(new Minus, new Plus, new Hook, new Pipe, new Dot)
  private def left(rock: Set[Pos]): Pos = rock.minBy(_._2)
  private def right(rock: Set[Pos]): Pos = rock.maxBy(_._2)

  private val width = 7L

  private def keepTopTocks(rocks: Set[Pos]): Set[Pos] = {
    // Any shorter does not seal the cave (resulting in the PIPE rock to fall into oblivion, aka an endless loop.)
    val top = rocks.maxBy(_._1)._1 - 30
    rocks.filter(_._1 > top)
  }

  @tailrec
  private def dropRocks(topRocks: Set[Pos], rockNr: Long, numberOfRocks: Long, aitJets: String, jetNr: Long): Set[Pos] = {
    @tailrec
    def dropRock(rock: Set[Pos], jetNr: Long): (Set[Pos], Long) = {
      val horizontalDirection = aitJets((jetNr % aitJets.length).toInt) match {
        case '<' => if (left(rock)._2 > 0) -1L else 0L
        case '>' => if (right(rock)._2 < (width - 1)) 1L else 0L
      }
      val jRock = rock.map(r => (r._1, r._2 + horizontalDirection))
      val hRock = if (topRocks.intersect(jRock).isEmpty) jRock else rock
      val vRock = hRock.map(r => (r._1 - 1, r._2))
      if (topRocks.intersect(vRock).isEmpty) {
        dropRock(vRock, jetNr + 1L)
      } else
        (hRock, jetNr + 1L)
    }
    if (rockNr == numberOfRocks)
      topRocks
    else {
      val top = topRocks.map(_._1).max
      val rock = rocks((rockNr % rocks.length).toInt).shape.map(r => (r._1 + top  + 4L, r._2 + 2L))
      val (droppedRock, jets) = dropRock(rock, jetNr)
      dropRocks(keepTopTocks(topRocks ++ droppedRock), rockNr + 1L, numberOfRocks, aitJets, jets)
    }
  }

  override def exampleAnswerPart1: Long = 3068L
  override def solvePart1(lines: List[String]): Long = {
    val airJets  = lines.head.trim
    val rocks: Set[Pos] = (0L until width).map((0L, _)).toSet
     val topRocks: Set[Pos] = dropRocks(rocks, 0L, 2022L, airJets, 0L)
     topRocks.map(_._1).max
  }

  var repeating = 315L // For the example
  // After much playing around this pattern (as produced by keepTopRocks) reappeared consistently every 1689/1690 rocks
  // Pattern finding was done by putting the result fo keepTopRocks in a set. This showed there was a limited number
  // of patterns. Counting the occurrence of every pattern pointed into this pattern to be crucial.
  val MARKER = List(Set(), Set(14, 6, 13, 12, 18, 11, 19, 15), Set(20, 6, 13, 2, 18, 11, 19, 4, 15), Set(5, 14, 6, 13, 2, 12, 3, 11, 4, 15), Set(0, 10, 20, 1, 6, 9, 13, 2, 7, 18, 16, 11, 8, 19, 4, 15), Set(17, 7, 18, 16, 8, 19, 15), Set(16))
  override def exampleAnswerPart2: Long = 1514285714288L
  override def solvePart2(lines: List[String]): Long = {
    val airJets = lines.head.trim
    val rocks: Set[Pos] = (0L until width).map((0L, _)).toSet
    val totalRocks = 1000000000000L
    val rocksForBase = (totalRocks % repeating)
    val rocksForDelta = rocksForBase + repeating
    val base = dropRocks(rocks, 0L, rocksForBase, airJets, 0L).map(_._1).max
    val delta = dropRocks(rocks, 0L, rocksForDelta, airJets, 0L).map(_._1).max - base
    repeating = 1690L // Fot the real thing
    base + (delta * (totalRocks - rocksForBase) / (rocksForDelta - rocksForBase))
  }

}

object Day17 extends App{
  new Day17().solvePuzzles("/2022/day17.txt")
}
