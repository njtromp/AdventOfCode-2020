package nl.njtromp.adventofcode_2020

import nl.njtromp.{Day24BaseVisitor, Day24Lexer, Day24Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.annotation.tailrec
import scala.collection.JavaConverters

class Day24 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    flipTiles(lines.map(parseInstructions)).size
  }

  override def solvePart2(lines: List[String]): Long = {
    proceed(flipTiles(lines.map(parseInstructions)), 100).size
  }

  @tailrec
  private def proceed(floor: Set[(Int, Int)], days: Int): Set[(Int, Int)] = {
    if (days == 0) floor else
      proceed(
          floor.filter(tile => (1 to 2).contains((floor intersect neighbors(tile)).size)) // Black tiles staying black
        union
          floor.flatten(tile => (neighbors(tile) diff floor).filter(w => (floor intersect neighbors(w)).size == 2)) // White tiles becoming black
        , days - 1
      )
  }

  private def neighbors(pos: (Int, Int)): Set[(Int, Int)] = {
    Set(
      (pos._1 + 1, pos._2 + 1), // ne
      (pos._1 + 2, pos._2),     // e
      (pos._1 + 1, pos._2 - 1), // se
      (pos._1 - 1, pos._2 - 1), // sw
      (pos._1 - 2, pos._2),     // w
      (pos._1 - 1, pos._2 + 1)  // nw
    )
  }

  private def flipTiles(tilePattern: List[List[String]]): Set[(Int, Int)] = {
    def flip(floor: Set[(Int, Int)], tile: (Int, Int)): Set[(Int, Int)] = if (floor.contains(tile)) floor - tile else floor + tile
    tilePattern.foldLeft(Set.empty[(Int, Int)])((floor, tileInstructions) =>
      flip(floor, tileInstructions.foldLeft((0, 0))((pos, instruction) =>
        instruction match {
          case "ne" => (pos._1 + 1, pos._2 + 1)
          case "e" => (pos._1 + 2, pos._2)
          case "se" => (pos._1 + 1, pos._2 - 1)
          case "sw" => (pos._1 - 1, pos._2 - 1)
          case "w" => (pos._1 - 2, pos._2)
          case "nw" => (pos._1 - 1, pos._2 + 1)
        }
      ))
    )
  }

  private def parseInstructions(line: String): List[String] = {
    StepsVisitor.visit(new Day24Parser(new CommonTokenStream(new Day24Lexer(CharStreams.fromString(line)))).steps())
  }

  object StepsVisitor extends Day24BaseVisitor[List[String]] {
    override def visitSteps(ctx: Day24Parser.StepsContext): List[String] = {
      JavaConverters.asScalaBuffer(ctx.step()).map(StepVisitor.visit(_)).toList
    }
  }

  object StepVisitor extends Day24BaseVisitor[String] {
    override def visitStep(ctx: Day24Parser.StepContext): String = ctx.getText
  }
}

object Day24 extends App {
  new Day24().solvePuzzles("/input-puzzle24.txt")
}
