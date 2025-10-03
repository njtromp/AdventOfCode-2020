package nl.njtromp.adventofcode

import scala.collection.mutable

class Day23 extends Puzzle[String] {

  private case class Computer(name: String, connections: Set[String]) {
    def isConnected(other: String): Boolean = connections.contains(other)
    def isChief: Boolean = name.startsWith("t")
    def connectionCombinations: List[(String, String)] =
      def combine(connections: Set[String]): List[(String, String)] =
        if connections.size <= 1 then
          List.empty
        else
          connections.tail.map((connections.head, _)).toList ++ combine(connections.tail)
      combine(connections)
  }

  private def parse(lines: List[String]): List[Computer] =
    lines.flatMap(parseConnection).groupBy(_._1)
      .map(c => (c._1, c._2.map(_._2).toSet)).map(this.Computer.apply)
      .toList

  private def parseConnection(line: String): Map[String, String] =
    val parts = line.split('-')
    Map(parts.head -> parts.last, parts.last -> parts.head)

  override def exampleAnswerPart1: String = "7"
  override def solvePart1(lines: List[String]): String =
    val computers = parse(lines)
    val byName =  computers.map(c => c.name -> c).toMap
    val chiefs = computers.filter(_.isChief)
    val triplets = chiefs.flatMap(c =>
      c.connectionCombinations
        .filter((a, b) => byName(a).isConnected(b))
        .map((a, b) => Set(c.name, a, b))
    ).toSet
    triplets.size.toString

  override def exampleAnswerPart2: String = "co,de,ka,ta"
  override def solvePart2(lines: List[String]): String =
    val computers = parse(lines)
    val byName =  computers.map(c => c.name -> c).toMap
    val result = byName.keySet.map(name =>
      byName(name).connectionCombinations
        .filter((a, b) => byName(a).isConnected(b))
        .toSet
        .flatMap((a, b) => Set(name, a, b))
    )
    result
      .filter(names =>
        val computer = byName(names.head)
        names.tail.forall(computer.connections.contains)
      )
      .maxBy(_.size).toList.sorted.mkString(",")

}

object Day23 extends App {
  new Day23().solvePuzzles()
}
