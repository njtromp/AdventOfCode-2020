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
  def fullMapping: Parser[FullMapping] = mapping ~ rep(numbers) ^^ { case names ~ values => FullMapping(names, values)}
  def puzzle: Parser[Puzzle] = seeds ~ rep(fullMapping) ^^ { case seeds ~ mappings => Puzzle(seeds, mappings) }

  case class Mapping(source: LongRange, destination: LongRange) {
    def isInRange(value: Long): Boolean = source.contains(value)
    def map(value: Long): Long = destination.first + (value - source.first)
    def map(r: LongRange): LongRange = if isInRange(r.first) then LongRange(map(r.first), map(r.last)) else r
  }
  case class FullMapping(names: (String, String), values: List[Mapping])
  case class Puzzle(seeds: List[Long], mappings: List[FullMapping])

  def split(source: LongRange, mappings: List[Mapping]): List[LongRange] =
    val candidateMappings = mappings.filter(_.source.isOverlapping(source))
    if candidateMappings.isEmpty then
      List(source)
    else
      val mapping = candidateMappings.minBy(_.source.first)
      source.split(mapping.source) match {
        case a :: b :: c :: Nil =>
          List(mapping.map(a), mapping.map(b)) ++ split(c, mappings)
        case a :: b :: Nil =>
          mapping.map(a) :: split(b, mappings)
        case a :: Nil =>
          List(mapping.map(a))
        case Nil =>
          List.empty
      }

  private def map(name: String, source: Long, nameMapping: Map[String, String], valueMapping: Map[String, List[Mapping]]): Long =
    @tailrec
    def mapper(name: String, source: Long): Long =
      if name == "location" then
        source
      else
        val destName = nameMapping(name)
        val range = valueMapping(name).filter(_.isInRange(source))
        mapper(destName, if range.isEmpty then source else range.head.map(source))
    mapper(name, source)

  private def map(name: String, sources: List[LongRange], nameMapping: Map[String, String], valueMapping: Map[String, List[Mapping]]): List[LongRange] =
    @tailrec
    def mapper(name: String, sources: List[LongRange]): List[LongRange] =
      if name == "location" then
        sources
      else
        mapper(nameMapping(name), sources.flatMap(split(_, valueMapping(name))))
    mapper(name, sources)

  override def exampleAnswerPart1: Long = 35
  override def solvePart1(lines: List[String]): Long =
    val input = parseAll(puzzle, lines.mkString("\n")).get
    val startingSeeds = input.seeds
    val nameMapping = input.mappings.map(m => m._1._1 -> m._1._2).toMap
    val valueMapping = input.mappings.map(m => m._1._1 -> m._2).toMap
    startingSeeds.map(map("seed", _, nameMapping, valueMapping)).min

  override def exampleAnswerPart2: Long = 46
  override def solvePart2(lines: List[String]): Long =
    val input = parseAll(puzzle, lines.mkString("\n")).get
    val startingSeeds = input.seeds
    val seedRanges = LongRange.combine(startingSeeds.sliding(2, 2).map(ns => LongRange(ns.head, ns.head + ns.last - 1)).toList)
    val nameMapping = input.mappings.map(m => m._1._1 -> m._1._2).toMap
    val valueMapping = input.mappings.map(m => m._1._1 -> m._2).toMap
    map("seed", seedRanges, nameMapping, valueMapping).minBy(_.first).first
}

object Day05B extends App {
  new Day05B().solvePuzzles("/day05.txt")
}
