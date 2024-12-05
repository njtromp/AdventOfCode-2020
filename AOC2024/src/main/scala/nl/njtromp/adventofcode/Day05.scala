package nl.njtromp.adventofcode

import scala.collection.mutable

class Day05 extends Puzzle[Long] {

  private def parseRules(rules: List[String]): List[(Long, Long)] =
    rules.map(rule =>
      val r = rule.split('|')
      r.head.toLong -> r.last.toLong
    )

  private def parseUpdates(updates: List[String]): List[List[Long]] =
    updates.map(_.split(',').map(_.toLong).toList)

  private def isValid(updates: List[Long], rules: List[(Long, Long)]): Boolean =
    val applicableRules = rules.filter(r => updates.contains(r._1) && updates.contains(r._2))
    applicableRules.forall(r => updates.indexOf(r._1) < updates.indexOf(r._2))

  private def correctOrder(updates: List[Long], rules: List[(Long, Long)]): List[Long] =
    val applicableRules = rules.filter(r => updates.contains(r._1) && updates.contains(r._2)).map(r => (r._1.toInt, r._2.toInt))
    val indexes = mutable.Map.empty[Long, Int]
    updates.zipWithIndex.foreach(u => indexes(u._1) = u._2)
    var brokenRules = applicableRules.filter(r => indexes(r._1) >= indexes(r._2))
    while brokenRules.nonEmpty do
      brokenRules.foreach(r => indexes(r._2) += 1)
      brokenRules = applicableRules.filter(r => indexes(r._1) >= indexes(r._2))
    indexes.toList.sortBy(_._2).map(_._1)

  override def exampleAnswerPart1: Long = 143
  override def solvePart1(lines: List[String]): Long =
    val grouped = groupByEmptyLine(lines)
    val rules = parseRules(grouped.head)
    val updates = parseUpdates(grouped.last)
    updates.filter(isValid(_, rules))
      .map(u => u(u.size / 2)).sum

  override def exampleAnswerPart2: Long = 123
  override def solvePart2(lines: List[String]): Long =
    val grouped = groupByEmptyLine(lines)
    val rules = parseRules(grouped.head)
    val updates = parseUpdates(grouped.last)
    updates.filterNot(isValid(_, rules))
      .map(correctOrder(_, rules))
      .map(u => u(u.size / 2)).sum

}

object Day05 extends App {
  new Day05().solvePuzzles()
}
