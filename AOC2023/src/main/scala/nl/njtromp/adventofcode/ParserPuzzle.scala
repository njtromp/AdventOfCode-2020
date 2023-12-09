package nl.njtromp.adventofcode

import scala.util.parsing.combinator.RegexParsers

trait ParserPuzzle[T] extends Puzzle[T] with RegexParsers {
  def integer: Parser[Long] = "-?\\d+".r ^^ { _.toLong }
  def integers: Parser[List[Long]] = rep(integer) ^^ { ints => ints }
  def word: Parser[String] = "\\w+".r ^^ { w => w }
  def words: Parser[List[String]] = rep(word) ^^ { ws => ws }

}
