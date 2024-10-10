package nl.njtromp.adventofcode_2020

import _root_.nl.njtromp.adventofcode.{Day18_2020BaseVisitor, Day18_2020Lexer, Day18_2020Parser}
import nl.njtromp.adventofcode.Puzzle
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

class Day18Antlr extends Puzzle {
  def solvePart1(lines: List[String]): Long = {
    lines.map(l => PuzzleVisitor.visit(new Day18_2020Parser(new CommonTokenStream(new Day18_2020Lexer(CharStreams.fromString(l)))).prog)).sum
  }

  def solvePart2(lines: List[String]): Long = {
    lines.map(l => PuzzleVisitor.visit(new Day18_2020Parser(new CommonTokenStream(new Day18_2020Lexer(CharStreams.fromString(l)))).prog2)).sum
  }

  object PuzzleVisitor extends Day18_2020BaseVisitor[Long] {
    override def visitProg(ctx: Day18_2020Parser.ProgContext): Long = visit(ctx.expr(0))

    override def visitExpr(ctx: Day18_2020Parser.ExprContext): Long = {
      ctx.getChildCount match {
        case 3 => ctx.getChild(1).getText match {
          case "+" => visit(ctx.getChild(0)) + visit(ctx.getChild(2))
          case "*" => visit(ctx.getChild(0)) * visit(ctx.getChild(2))
          case _ => visit(ctx.getChild(1))
        }
        case 1 => ctx.INT().getText.toLong
      }
    }

    override def visitProg2(ctx: Day18_2020Parser.Prog2Context): Long = visit(ctx.expr2(0))

    override def visitExpr2(ctx: Day18_2020Parser.Expr2Context): Long = {
      ctx.getChildCount match {
        case 3 => ctx.getChild(1).getText match {
          case "+" => visit(ctx.getChild(0)) + visit(ctx.getChild(2))
          case "*" => visit(ctx.getChild(0)) * visit(ctx.getChild(2))
          case _ => visit(ctx.getChild(1))
        }
        case 1 => ctx.INT().getText.toLong
      }
    }
  }

}

object Day18Antlr extends App {
  new Day18Antlr().solvePuzzles("/2020/input-puzzle18.txt")
}
