package nl.njtromp.adventofcode_2020


import nl.njtromp.adventofcode.Puzzle

import scala.util.parsing.combinator.RegexParsers

class Day18 extends Puzzle with RegexParsers {
  def number: Parser[Long] = "\\d+".r ^^ {_.toLong}

  def solvePart1(lines: List[String]): Long = {
    lines.map(eval1).sum
  }

  def solvePart2(lines: List[String]): Long = {
    lines.map(eval2).sum
  }

  private def eval1(expr: String): Long = {
    def factor: Parser[Long] = number | "(" ~> expression <~ ")"
    def expression: Parser[Long] = factor ~ rep( ("*" | "+") ~ factor) ^^ {
      case number ~ list => (list foldLeft number) {
        case (x, "+" ~ y) => x + y
        case (x, "*" ~ y) => x * y
      }
    }
    parseAll(expression, expr.replaceAll(" ", "")).get
  }

  private def eval2(expr: String): Long = {
    def factor: Parser[Long] = number | "(" ~> expression <~ ")"
    def term: Parser[Long] = factor ~ rep( "+" ~ factor) ^^ {
      case number ~ list => (list foldLeft number) {
        case (x, "+" ~ y) => x + y
      }
    }
    def expression: Parser[Long] = term ~ rep( "*" ~ term) ^^ {
      case number ~ list => (list foldLeft number) {
        case (x, "*" ~ y) => x * y
      }
    }
    parseAll(expression, expr.replaceAll(" ", "")).get
  }

}

object Day18 extends App {
  new Day18().solvePuzzles("/2020/input-puzzle18.txt")
}
