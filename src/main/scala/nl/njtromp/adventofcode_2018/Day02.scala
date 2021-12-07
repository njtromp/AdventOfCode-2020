package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle

class Day02 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    lines.flatMap(_.toList.groupBy(c => c).filter(c => List(2, 3).contains(c._2.length)).map(_._2.length).toSet)
      .groupBy(n => n).map(_._2.length).product
  }

  override def solvePart2(lines: List[String]): Long = {
    val ids = lines.filter(w => lines.count(m => w.zip(m).count(cs => cs._1 != cs._2) == 1) > 0)
    val bla: String = ids.reduce((w1, w2) => w1.zip(w2).filter(cs => cs._1 == cs._2).map(_._1).mkString)
    println(bla)
    0
  }
}


object Day02 extends App {
  new Day02().solvePuzzles("/2018/day02.txt")
}
