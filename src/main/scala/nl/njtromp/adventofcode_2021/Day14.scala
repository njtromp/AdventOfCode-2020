package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec
import scala.util.matching.Regex

class Day14 extends Puzzle {

  val Instruction: Regex = "(.)(.) -> (.)".r

  @tailrec
  private def evolve(step: Int, polymerTemplate: String, insertions: Map[(Char, Char), Char]): String = {
    def oneStep(template: String): String = {
      if (template.length <= 1) {
        template
      } else {
        val a = template(0)
        val b = template(1)
        Array(a, insertions((a, b))).mkString + oneStep(template.tail)
      }
    }
    if (step == 0)
      polymerTemplate
    else {
      val newTemplate = oneStep(polymerTemplate)
      println("-"*20)
      println(newTemplate)
      evolve(step - 1, newTemplate, insertions)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val polymerTemplate = lines.head
    val insertions = lines.tail.filterNot(_.isBlank).map({case Instruction(a, b, c) => (a(0), b(0)) -> c(0)}).toMap
    val finalPolymer: String = evolve(10, polymerTemplate, insertions)
    val polymerCounts: Map[Char, Int] = finalPolymer.toArray.groupBy(c => c).map(c => (c._1, c._2.length))
    polymerCounts.values.max - polymerCounts.values.min
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day14 extends App {
  new Day14().solvePuzzles("/2021/day14.txt")
}
