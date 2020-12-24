package nl.njtromp.adventofcode_2020

import nl.njtromp.{Day24BaseVisitor, Day24Lexer, Day24Parser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.{JavaConverters, mutable}

class Day24 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val instructions = lines.map(parseInstructions)
    val diameter = 3 * instructions.map(_.size).max
    val floor: Array[Array[Boolean]] = Array.ofDim(diameter, diameter)

    flipTiles(floor, instructions, diameter)

    floor.map(_.count(_ == true)).sum
  }

  override def solvePart2(lines: List[String]): Long = {
    val instructions = lines.map(parseInstructions)
    val diameter = 3 * instructions.map(_.size).max + 400 // 400 is every day expanding is each direction plus safety margin
    var floor: Array[Array[Boolean]] = Array.ofDim(diameter, diameter)

    flipTiles(floor, instructions, diameter)

    var days = 100
    while (days > 0) {
      val newFloor: Array[Array[Boolean]] = Array.ofDim(diameter, diameter)
      var y = 1
      while (y < floor.length - 1) {
        var x = 2 + (y % 2)
        while (x < floor(y).length - 3) {
          newFloor(y)(x) = if (floor(y)(x)) (1 to 2).contains(countBlackNeighbors(floor, x, y)) else countBlackNeighbors(floor, x, y) == 2
          x += 2
        }
        y += 1
      }
      floor = newFloor
      days -= 1
    }

    floor.map(_.count(_ == true)).sum
  }

  def countBlackNeighbors(floor: Array[Array[Boolean]], x: Int, y: Int): Int = {
    List(
      floor(y + 1)(x + 1),  // ne
      floor(y)(x + 2),      // e
      floor(y - 1)(x + 1),  // se
      floor(y - 1)(x - 1),  // sw
      floor(y)(x - 2),      // w
      floor(y + 1)(x - 1)   //nw
    ).count(_ == true)
  }

  private def flipTiles(floor: Array[Array[Boolean]], instructions: List[List[String]], diameter: Int): Unit = {
    instructions.foreach(instruction => {
      var x = diameter / 2
      var y = diameter / 2
      instruction.foreach {
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
      floor(y)(x) = !floor(y)(x)
    })
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
