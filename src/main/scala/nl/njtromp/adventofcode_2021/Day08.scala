package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day08 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val bla = lines.map(_.split("\\|")(1).split(" ").map(_.trim).filterNot(_.isBlank))
    bla.flatMap(_.map(_.length)).count(List(2, 4, 3, 7).contains(_))
  }

  private val segment = "abcdefg"
  private val validSegments = List("dabcge", "ab", "dafgc", "dafbc", "efab", "defbc", "defbcg", "dab", "abcdefg", "dafebc").map(_.sorted)
  override def solvePart2(lines: List[String]): Long = {
    def decodeLine(line: String): Long = {
      def cleanInput(ps: Array[String]): Array[String] = ps.map(_.trim).filterNot(_.isBlank)
      def decode(segments: Array[String], digits: Array[String]): Long = {
        @tailrec
        def deduceMapping(mapping: List[String]): Map[String, Int] = {
          if (mapping.isEmpty)
            Map.empty[String, Int]
          else {
            def isValidMapping(mapping: String): Boolean = {
              val segmentMapping = mapping.zip(segment).toMap
              segments.map(s => s.toList.map(segmentMapping(_)).mkString.sorted).count(validSegments.contains(_)) == 10
            }
            def createMappings(mapping: String): Map[String, Int] = {
              digits.map(s => {
                val segmentMapping = mapping.zip(segment).toMap
                (s.sorted, validSegments.indexOf(s.toList.map(segmentMapping(_)).mkString.sorted))
              })
            }.toMap
            if (isValidMapping(mapping.head))
              createMappings(mapping.head)
            else
              deduceMapping(mapping.tail)
          }
        }
        val mapping: Map[String, Int] = deduceMapping("abcdefg".permutations.toList)
        digits.foldLeft(0)((a, d) => a * 10 + mapping(d.sorted))
      }
      decode(cleanInput(line.split("\\|")(0).split(" ")), cleanInput(line.split("\\|")(1).split(" ")))
    }
    lines.map(decodeLine).sum
  }
}

object Day08 extends App{
  new Day08().solvePuzzles("/2021/day08.txt")
}
