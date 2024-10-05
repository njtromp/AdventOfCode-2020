package nl.njtromp.adventofcode

import scala.util.matching.Regex

class Day12 extends Puzzle[Long] {
  private case class Record(springs: String, damageCounts: List[Int]) {
    lazy val cleaned: String = shrink(springs)
  }

  private def parseLine(line: String): Record =
    val parts = line.split(' ')
    Record(shrink(parts.head), parts.last.split(',').map(_.toInt).toList)

  private def shrink(springs: String): String =
    springs.replaceAll("\\.+", "\\.").replaceAll("(^\\.+)|(\\.+$)", "")

  private def findMatches(text: String, regexs: List[Regex]): List[Int] =
    def findMatches(text: String, offset: Int, regex: Regex): Set[Int] =
      regex.findFirstMatchIn(text) match {
        case Some(m) =>
          findMatches(text.drop(1), offset + 1, regex) + (m.start + offset + 1)
        case None =>
          Set.empty
      }
    val headMatch = regexs.head.findFirstMatchIn(text)
    val midMatches = findMatches(text, 0, regexs(1)).toList.sorted
    val tailMatch = regexs.last.findFirstMatchIn(text)
    (headMatch, tailMatch) match {
      case (None, None) =>
        midMatches
      case (Some(h), None) =>
        h.start :: midMatches.filter(_ > h.start)
      case (None, Some(t)) =>
        midMatches.filter(_ < t.start + 1) ++ List(t.start + 1)
      case (Some(h), Some(t)) =>
        h.start :: midMatches.filter(m => m > h.start && m < t.start + 1) ++ List(t.start + 1)
    }

  private def countPermutations(record: Record): Long =
    def findLava(text: String): Set[Int] = text.zipWithIndex.filter(_._1 == '#').map(_._2).toSet
    def countPermutations(matches: List[List[Int]], lengths: List[Int], pattern: String): Long =
      matches match {
        case Nil =>
          0
        case ms :: Nil =>
          if ms.nonEmpty then
            val matchingPatterns = ms.map(m => pattern + "." * (m - pattern.length) + "#" * lengths.head)
              .filter(p =>
                val fixed = findLava(record.cleaned.substring(0, p.length))
                fixed.isEmpty || (findLava(p) intersect fixed).size == fixed.size
              )
            matchingPatterns.length
          else
            0
        case ms :: tail =>
          ms.map(m =>
            val filtered = tail.map(_.filter(_ > m + lengths.head))
            if filtered.nonEmpty then
              val newPattern = pattern + "." * (m - pattern.length) + "#" * lengths.head
              countPermutations(filtered, lengths.tail, newPattern)
            else
              0
          ).sum
      }
    val matches = record.damageCounts.indices.map(i =>
      val length = record.damageCounts(i)
      val sep = "(\\.|\\?)"
      val matcher = s"([#|\\?]{$length})"
      val regexs = List(
        s"^($matcher$sep)".r,
        s"($sep$matcher$sep)".r,
        s"($sep$matcher)$$".r
      )
      val matches = findMatches(record.cleaned, regexs)
//      println(matches)
      matches
    ).toList
//    println(record.cleaned)
    countPermutations(matches, record.damageCounts, "")

  private def unfold(record: Record): Record =
    Record((record.springs + '?').repeat(5).dropRight(1), (1 to 5).toList.flatMap(_ => record.damageCounts))

  override def exampleAnswerPart1: Long = (1 + 4 + 1 + 1 + 4 + 10)
  override def solvePart1(lines: List[String]): Long = {
    val bla = lines.map(parseLine).map(countPermutations)
//    println(bla)
    bla.sum
  }

  override def exampleAnswerPart2: Long = 525152
  override def solvePart2(lines: List[String]): Long =
//    println("\n\n")
    lines.map(l => unfold(parseLine(l))).map(countPermutations).sum
//    -1

}

object Day12 extends App {
  new Day12().solvePuzzles()
}
