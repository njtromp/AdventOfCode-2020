package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day12 extends Puzzle[Long] {
  private case class Record(springs: String, groups: List[Int]) {
    lazy val reduced: String = reduceWorkingSprings(springs)
    lazy val cleaned: Record = removeMatchingEndGroups(Record(reduced, this.groups))
    // If a group at either sides matches exactly it can be removed
    @tailrec
    private def removeMatchingEndGroups(record: Record): Record =
      def cleanHead(record: Record): Record =
        if record.groups.isEmpty then
          record
        else
          val headMatch = s"^(([#|\\?]{${record.groups.head}})(\\.))".r
          val head = record.reduced.take(record.groups.head + 1)
          val tail = record.reduced.drop(record.groups.head + 1)
          if headMatch.matches(head) && head.contains("#") then {
            Record(reduceWorkingSprings(tail), record.groups.tail)
          }
          else
            record
      def cleanTail(record: Record): Record =
        if record.groups.isEmpty then
          record
        else
          val tailMatch = s"((\\.)([#|\\?]{${record.groups.last}}))$$".r
          val tail = record.reduced.takeRight(record.groups.last + 1)
          val head = record.reduced.dropRight(record.groups.last + 1)
          if tailMatch.matches(tail) && tail.contains("#") then
            Record(reduceWorkingSprings(head), record.groups.dropRight(1))
          else
            record
      val cleaned = cleanHead(cleanTail(record))
      if cleaned == record then
        record
      else
        removeMatchingEndGroups(cleaned)
  }

  private def parseLine(line: String): Record =
    val parts = line.split(' ')
    Record(parts.head, parts.last.split(',').map(_.toInt).toList)

  // Reduces multiple working springs to just 1 and removes any working springs on the left and right side
  private def reduceWorkingSprings(springs: String): String =
    springs.replaceAll("\\.+", "\\.").replaceAll("(^\\.+)|(\\.+$)", "")

  private def countCombinations(record: Record): Long =
    val cache = mutable.Map.empty[(String, List[Int]), Long]
    def countCombinations(springs: String, groups: List[Int]): Long = {
      val key = (springs, groups)
      if cache.contains(key) then
        cache(key)
      else
        val result: Long =
          if groups.isEmpty then
            if !springs.contains('#') then 1 else 0
          else if springs.isEmpty then
            0
          else
            val springSize = springs.length
            val headGroupSize = groups.head
            val tailSize = if groups.tail.isEmpty then 0 else groups.tail.sum + groups.tail.size - 1
            (0 to springSize - headGroupSize - tailSize)
              .filter(n =>
                val headMatch = if n == springSize - headGroupSize - tailSize then
                  s"^(([\\.\\?]{$n})([#\\?]{$headGroupSize}))$$".r
                else if n == 0 then
                  s"^([#\\?]{$headGroupSize})([\\.\\?])(.*)".r
                else
                  s"^(([\\.\\?]{$n})([#\\?]{$headGroupSize}))([\\.\\?])(.*)".r
                headMatch.matches(springs)
              )
              .map(n =>
                val extra = if n + headGroupSize + 1 <= springSize then 1 else 0
                val tail = springs.drop(n + headGroupSize + extra)
                // Remove any leading dots (= working spring)
                countCombinations(tail.dropWhile(_ == '.'), groups.tail)
              ).sum
        cache(key) = result
        result
    }
    countCombinations(record.cleaned.springs, record.cleaned.groups)

  private def unfold(record: Record): Record =
    val unfoldCount = 5
    Record((record.springs + '?').repeat(unfoldCount).dropRight(1), (1 to unfoldCount).toList.flatMap(_ => record.groups))

  override def exampleAnswerPart1: Long = (1 + 4 + 1 + 1 + 4 + 10)
  override def solvePart1(lines: List[String]): Long =
    lines.map(parseLine)
      .map(countCombinations)
      .sum

  override def exampleAnswerPart2: Long = 525152
  override def solvePart2(lines: List[String]): Long =
    lines.map(l => unfold(parseLine(l)))
      .map(countCombinations)
      .sum

}

object Day12 extends App {
  new Day12().solvePuzzles()
}