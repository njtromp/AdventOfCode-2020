package nl.njtromp.adventofcode_2020

import scala.io.Source
import scala.util.matching.Regex

object Day02 extends App {
  var validPasswordsPart1 = 0
  var validPasswordsPart2 = 0
  val pattern: Regex = "(\\d+)-(\\d+) (.): (.+)".r
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/2020/input-puzzle02.txt")).getLines) {
    for (parts <- pattern.findAllMatchIn(line)) {
      val min = parts.group(1).toInt
      val max = parts.group(2).toInt
      val c = parts.group(3)
      val password = parts.group(4)
      val charCount = password.replaceAll(s"[^$c]", "").length
      if (charCount >= min && charCount <= max) {
        validPasswordsPart1 += 1
      }
      if ((password.charAt(min - 1) == c.charAt(0) ^ password.charAt(max - 1) == c.charAt(0))){
        validPasswordsPart2 += 1
      }
    }
  }
  println(s"Answer part 1: $validPasswordsPart1")
  println(s"Answer part 2: $validPasswordsPart2")
}
