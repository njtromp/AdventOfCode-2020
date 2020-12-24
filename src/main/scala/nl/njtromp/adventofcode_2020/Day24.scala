package nl.njtromp.adventofcode_2020

import nl.njtromp.{Day24BaseVisitor, Day24Lexer, Day24Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.{JavaConverters, mutable}

class Day24 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val instructions = lines.map(parseInstructions)
    val diameter = 3 * instructions.map(_.size).max
    val floor: Array[Array[Boolean]] = Array.ofDim(diameter, diameter)

    instructions.foreach(instruction => {
      var x = diameter / 2
      var y = diameter / 2
      instruction.foreach(step => {
        step match {
          case "ne" =>
            x += 1
            y += 1
          case "e" =>
            x += 2
          case "se" =>
            x += 1
            y -= 1
          case "sw" =>
            x -= 1
            y -= 1
          case "w" =>
            x -= 2
          case "nw" =>
            x -= 1
            y += 1
        }
      })
      floor(x)(y) = !floor(x)(y)
    })

    floor.map(_.count(_ == true)).sum
  }

  override def solvePart2(lines: List[String]): Long = {
    -1
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
