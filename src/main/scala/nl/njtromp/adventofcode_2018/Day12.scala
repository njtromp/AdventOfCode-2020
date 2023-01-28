package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day12(days1: Long, days2: Long) extends Puzzle2 {

  private def neighbors(pot: Long): Set[Long] = Set(pot - 2, pot - 1, pot, pot + 1, pot + 2)

  private def key(pot: Long, pots: Set[Long]): Set[Long] = pots.map(_ - pot)

  @tailrec
  private def evolve(pots: Set[Long], rules: Map[Set[Long], Boolean], day: Long): Long = {
//    println(s"${pots.size} (${pots.min}, ${pots.max})")
//    println((pots.min - 2 to pots.max + 2).map(p => if (pots.contains(p)) "#" else ".").mkString)
    if (day == 0) pots.sum else
      evolve(
        pots.flatMap(neighbors).map(p => (p, key(p, neighbors(p) intersect pots))).filter(p => rules.contains(p._2) && rules(p._2)).map(_._1)
        , rules, day - 1)
  }

  override def exampleAnswerPart1: Long = 325
  override def solvePart1(lines: List[String]): Long = {
    evolve(lines.head.split(": ")(1).zipWithIndex.filter(_._1 == '#').map(_._2.toLong).toSet,
      lines.tail.map(r => (r.split(" => ")(0).zipWithIndex.filter(_._1 == '#').map(_._2.toLong - 2).toSet, r.split(" => ")(1) == "#")).toMap,
      days1
    )
  }

  override def exampleAnswerPart2: Long = 999999999374L
  override def solvePart2(lines: List[String]): Long = {
    val first1000 = evolve(lines.head.split(": ")(1).zipWithIndex.filter(_._1 == '#').map(_._2.toLong).toSet,
      lines.tail.map(r => (r.split(" => ")(0).zipWithIndex.filter(_._1 == '#').map(_._2.toLong - 2).toSet, r.split(" => ")(1) == "#")).toMap,
      150L
    )
    val perDay = evolve(lines.head.split(": ")(1).zipWithIndex.filter(_._1 == '#').map(_._2.toLong).toSet,
      lines.tail.map(r => (r.split(" => ")(0).zipWithIndex.filter(_._1 == '#').map(_._2.toLong - 2).toSet, r.split(" => ")(1) == "#")).toMap,
      151L
    ) - first1000
    first1000 + (days2 - 150L) * perDay
  }

}

object Day12 extends App {
  new Day12(20, 50000000000L).solvePuzzles("/2018/day12.txt")
}
