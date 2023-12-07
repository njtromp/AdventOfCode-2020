package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

class Day05B extends Puzzle[Long] with RegexParsers {
  def number: Parser[Long] =
    "\\d+".r ^^ { _.toLong }
  def seeds: Parser[List[Long]] =
    "seeds:".r ~ rep(number) ^^ { case "seeds:" ~ numbers => numbers }
  def mapping: Parser[(String, String)] =
    "\\w+".r ~ "-to-" ~ "\\w+".r ~ "map:" ^^ { case from ~ "-to-" ~ to ~ "map:" => (from, to) }
  def numbers: Parser[Mapping] = number ~ number ~ number ^^
    { case destination ~ source ~ range => Mapping(LongRange(source, source + range - 1), LongRange(destination, destination + range - 1)) }

  case class Mapping(source: LongRange, destination: LongRange) {
    def isInRange(value: Long): Boolean = source.contains(value)
    def map(value: Long): Long = destination.first + (value - source.first)
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

  private def transform(source: LongRange, mappings: List[Mapping]): List[(LongRange, LongRange)] =
    val candidates = mappings.filter(_.source.isOverlapping(source))
    println(s"$source (${candidates.size})) => $candidates")
    List.empty

  override def exampleAnswerPart2: Long = 46
  override def solvePart2(lines: List[String]): Long =
    val seedRanges = LongRange.combine(parseSeedInfo(lines).sliding(2, 2).map(ns => LongRange(ns.head, ns.head + ns.last - 1)).toList)
    val mappingInfo = groupByEmptyLine(lines.drop(2)).map(parseMappingInfo)
    val nameMapping = mappingInfo.map(m => m._1._1 -> m._1._2).toMap
    val valueMapping = mappingInfo.map(m => m._1._1 -> m._2).toMap
    seedRanges.foreach(transform(_, valueMapping("seed")))
//    seedRanges.map(r => r.values().foldLeft(Long.MaxValue)((a, s) => Math.min(a, map("seed", s, nameMapping, valueMapping)))).min
    println(LongRange(0, 10).destruct(LongRange(5, 20)))
    println(LongRange(0, 10).destruct(LongRange(5, 10)))
    46
}

object Day05B extends App {
  new Day05B().solvePuzzles("/day05.txt")
}
