package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

class Day05 extends Puzzle[Long] with RegexParsers {
  def number: Parser[Long] = """\d+""".r ^^ { _.toLong }
  def seeds: Parser[List[Long]] = """seeds:""".r ~ rep(number) ^^ { case "seeds:" ~ numbers => numbers }
  def mapping: Parser[(String, String)] = """\w+""".r ~ "-to-" ~ """\w+""".r ~ "map:" ^^ { case from ~ "-to-" ~ to ~ "map:" => (from, to) }
  def numbers: Parser[Mapping] = number ~ number ~ number ^^ {case destStart ~ sourceStart ~ range => Mapping(sourceStart, destStart, range) }

  case class Mapping(sourceStart: Long, destinationStart: Long, range: Long) {
    def isInRange(source: Long): Boolean = source >= sourceStart && source < sourceStart + range
    def map(source: Long): Long = source + (destinationStart - sourceStart)
  }

  private def parseSeedInfo(lines: List[String]): List[Long] =
    parse(seeds, lines.head) match { case Success(seeds, _) => seeds }

  private def parseMappingInfo(lines: List[String]): ((String, String), List[Mapping]) =
    (
      parse(mapping, lines.head) match { case Success(mapping, _) => mapping },
      lines.tail.map(parse(numbers, _) match { case Success(mapping, _) => mapping })
    )

  private def map(name: String, source: Long, nameMapping: Map[String, String], valueMapping: Map[String, List[Mapping]]): Long =
    @tailrec
    def mapper(name: String, source: Long): Long =
      if name == "location" then
        source
      else
        val destName = nameMapping(name)
        val range = valueMapping(name).filter(_.isInRange(source))
        val destination = if range.isEmpty then source else range.head.map(source)
        mapper(destName, destination)
    mapper(name, source)

  override def exampleAnswerPart1: Long = 35
  override def solvePart1(lines: List[String]): Long =
    val startingSeeds = parseSeedInfo(lines)
    val mappingInfo = groupByEmptyLine(lines.drop(2)).map(parseMappingInfo)
    val nameMapping = mappingInfo.map(m => m._1._1 -> m._1._2).toMap
    val valueMapping = mappingInfo.map(m => m._1._1 -> m._2).toMap
    startingSeeds.map(map("seed", _, nameMapping, valueMapping)).min

  override def exampleAnswerPart2: Long = 46
  override def solvePart2(lines: List[String]): Long =
    val seedRanges = LongRange.combine(parseSeedInfo(lines).sliding(2, 2).map(ns => LongRange(ns.head, ns.head + ns.last - 1)).toList)
    val mappingInfo = groupByEmptyLine(lines.drop(2)).map(parseMappingInfo)
    val nameMapping = mappingInfo.map(m => m._1._1 -> m._1._2).toMap
    val valueMapping = mappingInfo.map(m => m._1._1 -> m._2).toMap
    seedRanges.map(r => r.values().foldLeft(Long.MaxValue)((a, s) => Math.min(a, map("seed", s, nameMapping, valueMapping)))).min
}

object Day05 extends App {
  new Day05().solvePuzzles()
}
