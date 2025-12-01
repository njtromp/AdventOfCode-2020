package nl.njtromp.adventofcode

class Day01 extends Puzzle[Long] {

  override def exampleAnswerPart1: Long = 3
  override def solvePart1(lines: List[String]): Long = {
    lines.foldLeft((50L, 0L))((a, l) =>
      val rotation = (if l.charAt(0) == 'L' then -1L else 1L) * l.substring(1).toLong
      val dial = (a._1 + rotation + 100L) % 100L
      (dial, a._2 + (if dial == 0 then 1 else 0))
    )._2
  }

  override def exampleAnswerPart2: Long = 6
  override def solvePart2(lines: List[String]): Long = {
    println("6183 is too high")
    def rotate(dial: Long, rotation: Long): Long =
      if rotation > 0 then {
        val newDial = (dial + 101L) % 100L
        if newDial == 0 then
          1 + rotate(newDial, rotation - 1)
        else
          rotate(newDial, rotation - 1)
      } else if rotation < 0 then
        val newDial = (dial + 99L) % 100L
        if newDial == 0 then
          1 + rotate(newDial, rotation + 1)
        else
          rotate(newDial, rotation + 1)
      else
        0

    lines.foldLeft((50L, 0L))((a, l) =>
      val rotation = (if l.charAt(0) == 'L' then -1L else 1L) * l.substring(1).toLong
      val dial = (a._1 + rotation + 100L) % 100L
      (dial, a._2 + rotate(a._1, rotation))
    )._2
  }

}

object Day01 extends App {
  new Day01().solvePuzzles()
}
