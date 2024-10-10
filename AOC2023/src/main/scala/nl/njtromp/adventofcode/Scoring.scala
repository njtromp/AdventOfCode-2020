package nl.njtromp.adventofcode

import java.time.{LocalDateTime, ZoneOffset}
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class Scoring extends RegexParsers {
  private case class Star(nr: Long, starIndex: Long, starDateTime: LocalDateTime)
  private case class Day(day: Long, stars: List[Star])
  private case class Player(id: Long)

  private def DOUBLE_QUOTE = "\""
  private def CURLY_OPEN = "{"
  private def CURLY_CLOSE = "}"
  private def COLON = ":"
  private def COMMA = ","
  private def GET_STAR_TS = "\"get_star_ts\""
  private def STAR_INDEX = "\"star_index\""
  private def PLAYER_ID = "\"\\w+\"".r
  private def STARS = "\"stars\""

  private def number: Parser[Long] = "\\d+".r ^^ { _.toLong }
  private def starInfo: Parser[Star] =
    (DOUBLE_QUOTE ~> number <~ (DOUBLE_QUOTE ~ COLON ~ CURLY_OPEN ~ GET_STAR_TS ~ COLON)) ~ number ~ ((COMMA ~ STAR_INDEX ~ COLON) ~> number ) <~ CURLY_CLOSE ^^
      { case id ~ time ~ star => Star(id, star, LocalDateTime.ofEpochSecond(time, 0, ZoneOffset.UTC)) }
  private def day: Parser[Day] =
    (DOUBLE_QUOTE ~> number <~ (DOUBLE_QUOTE ~ COLON ~ CURLY_OPEN)) ~ repsep(starInfo, ",") <~ CURLY_CLOSE ^^
      { case day ~ stars =>  Day(day, stars)}
  private def player: Parser[Player] = (PLAYER_ID ~ COLON ~ CURLY_OPEN ~ STARS ~ COLON ) ~> number ^^
    { case playerId => Player(playerId) }
  private def players: Parser[List[Player]] = CURLY_OPEN ~> repsep(player, ",") <~ CURLY_CLOSE ^^ { players => players }

  def processScore(data: String): Unit =
    val info = parseAll(day, data).get
    println(info)
}

object Scoring extends App() {
  val inputName = "/day00.json"
  val data = Source.fromInputStream(getClass.getResourceAsStream(inputName)).getLines().toList.mkString("\n")
  new Scoring().processScore(data)
}
