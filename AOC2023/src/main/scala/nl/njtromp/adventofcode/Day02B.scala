package nl.njtromp.adventofcode

import scala.util.parsing.combinator.RegexParsers

class Day02B extends Puzzle[Long] with RegexParsers {
  def red = "red"
  def green = "green"
  def blue = "blue"

  case class Ball(color: String, count: Int)
  case class Game(id: Int, takes: List[List[Ball]]) {
    def sets: List[Map[String, Int]] = takes.map(_.map(b => b.color -> b.count).toMap.withDefaultValue(0))
    def maxBalls(color: String): Int = sets.map(_(color)).max
  }

  def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def color: Parser[String] = s"""$red|$blue|$green""".r ^^ { color => color }
  def ball: Parser[Ball] = number ~ color ^^ { case count ~ color => Ball(color, count) }
  def balls: Parser[List[Ball]] = repsep(ball, ",")
  def game: Parser[Game] = "Game" ~ number ~ ":" ~ repsep(balls, ";") ^^ { case "Game" ~ id ~ ":" ~ takes => Game(id, takes) }

  override def exampleAnswerPart1: Long = 8
  override def solvePart1(lines: List[String]): Long =
    val games = lines.map(l => parse(game, l) match { case Success(game, _) => game })
    games.filter(g => g.maxBalls(red) <= 12 && g.maxBalls(green) <= 13 && g.maxBalls(blue) <= 14).map(_.id).sum

  override def exampleAnswerPart2: Long = 2286
  override def solvePart2(lines: List[String]): Long =
    val games = lines.map(l => parse(game, l) match { case Success(game, _) => game })
    games.map(g => g.maxBalls(red) * g.maxBalls(green) * g.maxBalls(blue)).sum
}

object Day02B extends App {
  new Day02B().solvePuzzles()
}
