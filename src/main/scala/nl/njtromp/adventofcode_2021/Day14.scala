package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

class Day14 extends Puzzle {

  val Instruction: Regex = "(.)(.) -> (.)".r

  def createPatterns(line: String): List[String] = {
    if (line.length < 2)
      List.empty
    else
      line.take(2) :: createPatterns(line.tail)
  }

  def solve(steps: Int, lines: List[String]): Long = {
    val templatePattern = createPatterns(lines.head)
    val insertions = lines.tail.filterNot(_.isBlank).map({case Instruction(a, b, c) => s"$a$b" -> List(s"$a$c", s"$c$b")}).toMap
    val patterns: Set[String] = insertions.keys.flatMap(k => k :: insertions(k)).toSet
    val patternCount: mutable.HashMap[String, Long] = new mutable.HashMap[String, Long]() ++ patterns.map(c => c -> 0L).toMap

    @tailrec
    def evolve(step: Int, patternCount: mutable.HashMap[String, Long]): mutable.HashMap[String, Long] = {
      if (step == 0)
        patternCount
      else {
        val pattern = patternCount.filter(_._2 > 0).keys.toSet
        val newPatternCount = mutable.HashMap[String, Long]()
        patternCount.keys.foreach(k => {
          if (pattern.contains(k)) {
            insertions(k).foreach(p => {
              newPatternCount.update(p, patternCount(k) + newPatternCount.getOrElse(p, 0L))
            })
          }
        })
        evolve(step -1, newPatternCount)
      }
    }

    templatePattern.foreach(p => patternCount.update(p, patternCount(p) + 1L))
    val finalPattern = evolve(steps, patternCount)
    val charCount = finalPattern.toList.map(x => (x._1(0), x._2)).groupBy(_._1).mapValues(_.map(_._2).sum)
    charCount.values.max - charCount.values.min
  }

  override def solvePart1(lines: List[String]): Long = {
    solve(10, lines)
  }

  override def solvePart2(lines: List[String]): Long = {
    solve(40, lines) + 1L
  }

}

object Day14 extends App {
  new Day14().solvePuzzles("/2021/day14.txt")
}
