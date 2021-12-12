package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day12 extends Puzzle {
  trait Cave
  case class SmallCave(name: String) extends Cave
  case class LargeCave(name: String) extends Cave

  def createEdge(line: String): (Cave, Cave) = {
    def createCave(name: String): Cave =
      if (name.toUpperCase == name) LargeCave(name) else SmallCave(name)
    val names = line.split("-")
    (createCave(names(0)), createCave(names(1)))
  }

  def createPaths(path: List[Cave], finish: Cave, canVisitTwice: Option[Cave], visited: Set[Cave], adjLists: Map[Cave, List[Cave]]): List[List[Cave]] = {
    if (path.head == finish)
      List(path)
    else {
      (path.head, canVisitTwice) match {
        case (current: SmallCave, None) => adjLists(current).filterNot(visited.contains).flatMap(c => createPaths(c :: path, finish, None, visited + current, adjLists))
        case (current: SmallCave, Some(twice)) =>
          if (current == twice)
            adjLists(current).filterNot(visited.contains).flatMap(c => createPaths(c :: path, finish, None, visited, adjLists))
          else
            adjLists(current).filterNot(visited.contains).flatMap(c => createPaths(c :: path, finish, canVisitTwice, visited + current, adjLists))
        case (current: LargeCave, _) => adjLists(current).filterNot(visited.contains).flatMap(c => createPaths(c :: path, finish, canVisitTwice, visited, adjLists))
      }
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val edges = lines.map(createEdge)
    val adjLists = edges.flatMap(s => List(s._1 -> s._2, s._2 -> s._1)).groupBy(_._1).mapValues(ds => ds.map(_._2))
    createPaths(List(SmallCave("start")), SmallCave("end"), None, Set.empty[Cave], adjLists).size
  }

  override def solvePart2(lines: List[String]): Long = {
    val edges = lines.map(createEdge)
    val adjLists = edges.flatMap(s => List(s._1 -> s._2, s._2 -> s._1)).groupBy(_._1).mapValues(ds => ds.map(_._2))
    val smallCaves = edges.flatMap(s => List(s._1, s._2)).toSet.filter(c => c match {
      case SmallCave(_) => true
      case LargeCave(_) => false
    }).filterNot(List(SmallCave("start"), SmallCave("end")).contains).toList
    smallCaves.flatMap(c => createPaths(List(SmallCave("start")), SmallCave("end"), Some(c), Set.empty[Cave], adjLists)).toSet.size
  }
}

object Day12 extends App {
  new Day12().solvePuzzles("/2021/day12.txt")
}
