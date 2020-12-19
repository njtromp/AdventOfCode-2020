package nl.njtromp.adventofcode_2020

import scala.collection.mutable
import scala.util.matching.Regex

class Day19 extends Puzzle {
  def solvePart1(lines: List[String]): Long = {
    var buildRules = true
    var baseRule = ""
    val rules: mutable.HashMap[Int, (List[Int], Option[List[Int]])] = mutable.HashMap.empty
    val chars: mutable.HashMap[Int, Char] = mutable.HashMap.empty
    var finalRule: Regex = "".r
    var matchingMessage: Long = 0
    for (line <- lines) {
      if (buildRules) {
        if (line.isEmpty) {
          finalRule = s"^(${buildPattern(baseRule.strip().split(" ").map(_.toInt).toList, rules, chars)})$$".r
          buildRules = false
        } else {
          val ruleParts = line.split(":")
          val nr = ruleParts(0).toInt
          if (ruleParts(1).contains('"')) {
            val char = ruleParts(1).strip().split('"')(1)
            chars += (nr.toInt -> char.charAt(0))
          } else if (nr == 0) {
            baseRule = ruleParts(1).strip()
          } else {
            if (ruleParts(1).contains('|')) {
              val splittedRules = ruleParts(1).split('|')
              rules += (nr.toInt -> (splittedRules(0).strip().split(' ').map(_.toInt).toList, Some(splittedRules(1).strip().split(' ').map(_.toInt).toList)))
            } else {
              rules += (nr.toInt -> (ruleParts(1).strip().split(' ').map(_.toInt).toList, None))
            }
          }
        }
      } else {
        if (finalRule.findAllMatchIn(line).hasNext)
          matchingMessage += 1
      }
    }
    matchingMessage
  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

  def buildPattern(nrs: List[Int], rules: mutable.HashMap[Int, (List[Int], Option[List[Int]])], chars: mutable.HashMap[Int, Char]): String = {
    if (nrs.isEmpty) {
      ""
    } else {
     s"(${buildPattern(nrs.head, rules, chars)}${buildPattern(nrs.tail, rules, chars)})"
    }
  }

  def buildPattern(nr: Int, rules: mutable.HashMap[Int, (List[Int], Option[List[Int]])], chars: mutable.HashMap[Int, Char]): String = {
    if (chars.contains(nr)) {
      chars(nr).toString
    } else {
      rules(nr) match {
        case (rs, None) => rs.map(r => buildPattern(r, rules, chars)).mkString
        case (rs, Some(os)) => s"((${buildPattern(rs, rules, chars)})|(${buildPattern(os, rules, chars)}))"
      }
    }
  }
  }

object Day19 extends App {
  new Day19().solvePuzzles("/input-puzzle19.txt")
}
