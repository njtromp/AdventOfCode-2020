package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

class Day08 extends Puzzle[Long] with RegexParsers {
  private def word: Parser[String] = "\\w+".r ^^ { w => w }
  private def mapping: Parser[(String, (String, String))] = word ~ "=" ~ "(" ~ word ~ "," ~ word ~ ")" ^^ { case from ~ "=" ~ "(" ~ left ~ "," ~ right ~ ")" => (from, (left, right)) }
  private def puzzle: Parser[Puzzle] = word ~ rep(mapping) ^^ { case directions ~ mappings => Puzzle(directions, mappings) }
  private def puzzles: Parser[List[Puzzle]] = rep(puzzle) ^^ { ps => ps }

  case class Puzzle(directions: String, mappings: List[(String, (String, String))])

  private def navigate(current: String, finished: String => Boolean, directions: String, mappings: Map[String, (String, String)]): Long =
    @tailrec
    def navigate(current: String, direction: String, steps: Long): Long =
      if finished(current) then
        steps
      else if direction.isEmpty then
          navigate(current, directions, steps)
      else
        val target = if direction.head == 'L' then mappings(current)._1 else mappings(current)._2
        navigate(target, direction.tail, steps + 1)
    navigate(current, directions, 0)

  override def exampleAnswerPart1: Long = 6
  override def solvePart1(lines: List[String]): Long =
    val inputs = parseAll(puzzles, lines.mkString("\n")).get
    val directions = inputs.head.directions
    val mappings = inputs.head.mappings.map(m => m._1 -> m._2).toMap
    navigate("AAA", _ == "ZZZ", directions, mappings)

  override def exampleAnswerPart2: Long = 6
  override def solvePart2(lines: List[String]): Long =
    val inputs = parseAll(puzzles, lines.mkString("\n")).get
    val directions = inputs.last.directions
    val mappings = inputs.last.mappings.map(m => m._1 -> m._2).toMap
    val starts = mappings.keys.filter(_.endsWith("A")).toList
    def divideIfMultiple(a: Long, b: Long): Long = if a % b == 0 then a / b else a
    starts.map(s => divideIfMultiple(navigate(s, _.endsWith("Z"), directions, mappings), directions.length)).product * directions.length
}

object Day08 extends App {
  new Day08().solvePuzzles()
}
