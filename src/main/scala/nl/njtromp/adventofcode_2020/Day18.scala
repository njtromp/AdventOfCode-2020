package nl.njtromp.adventofcode_2020

import scala.util.parsing.combinator.RegexParsers

class Day18 extends Puzzle with RegexParsers {
  def number: Parser[Long] = "\\d+".r ^^ {_.toLong}
  def factor: Parser[Long] = number | "(" ~> expression <~ ")"
  def expression: Parser[Long] = factor ~ rep( ("*" | "+") ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "*" ~ y) => x * y
    }
  }

//  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
//  def factor: Parser[Double] = number | "(" ~> expr <~ ")"
//  def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
//    case number ~ list => (number /: list) {
//      case (x, "*" ~ y) => x * y
//      case (x, "/" ~ y) => x / y
//    }
//  }
//  def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
//    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
//      case (x, "+" ~ y) => x + y
//      case (x, "-" ~ y) => x - y
//    }
//  }




  def solvePart1(lines: List[String]): Long = {
    lines.map(eval).sum
  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

  private def eval(expr: String): Long = {
    val cleanExpr: String = expr.replaceAll(" ", "")
    parseAll(expression, cleanExpr).get
  }

}

object Day18 extends App {
  new Day18().solvePuzzles("/input-puzzle18.txt")
}

