package nl.njtromp.adventofcode

class Day02B extends ParserPuzzle[Long] {
  def red = "red"
  def green = "green"
  def blue = "blue"

  case class Ball(color: String, count: Long)
  case class Game(id: Long, takes: List[List[Ball]]) {
    def sets: List[Map[String, Long]] = takes.map(_.map(b => b.color -> b.count).toMap.withDefaultValue(0))
    def maxBalls(color: String): Long = sets.map(_(color)).max
  }

  def color: Parser[String] = s"""$red|$blue|$green""".r ^^ { color => color }
  def ball: Parser[Ball] = integer ~ color ^^ { case count ~ color => Ball(color, count) }
  def balls: Parser[List[Ball]] = repsep(ball, ",")
  def game: Parser[Game] = "Game" ~ integer ~ ":" ~ repsep(balls, ";") ^^ { case "Game" ~ id ~ ":" ~ takes => Game(id, takes) }
  def gameList: Parser[List[Game]] = rep(game) ^^ { gs => gs }

  override def exampleAnswerPart1: Long = 8
  override def solvePart1(lines: List[String]): Long =
    val games = parse(gameList, lines.mkString("\n")).get
    games.filter(g => g.maxBalls(red) <= 12 && g.maxBalls(green) <= 13 && g.maxBalls(blue) <= 14).map(_.id).sum

  override def exampleAnswerPart2: Long = 2286
  override def solvePart2(lines: List[String]): Long =
    val games = parse(gameList, lines.mkString("\n")).get
    games.map(g => g.maxBalls(red) * g.maxBalls(green) * g.maxBalls(blue)).sum
}

object Day02B extends App {
  new Day02B().solvePuzzles("/day02.txt")
}
