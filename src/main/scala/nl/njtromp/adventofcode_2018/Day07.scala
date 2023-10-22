package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.StringPuzzle

class Day07 extends StringPuzzle {
  private class Node(id: Char, var next: List[Node])

  private def followPaths(nodes: List[Char], visited: Set[Char], finish: Char, next: Map[Char, List[Char]]): List[Char] = {
    nodes.sorted match {
      case Nil => List(finish)
      case c :: tail => if (c == finish) List(c) else c :: followPaths(tail ++ next(c).filterNot(visited.contains), visited + c, finish, next)
    }
  }

  private def removeDups(charaters: List[Char]): List[Char] = {
    charaters match {
      case Nil => Nil
      case c :: tail => List(c) ++ removeDups(tail.dropWhile(_ == c))
    }
  }

  override def exampleAnswerPart1: String = "CABDFE"
  override def solvePart1(lines: List[String]): String = {
    val order = lines.map(l => (l.charAt(5), l.charAt(36)))
    val next = order.groupMap(_._1)(_._2)
    val starts = order.map(_._1).toSet
    val finishes = order.map(_._2).toSet
    val finish = finishes.diff(starts).head
    starts.diff(finishes).toList.map(s => removeDups(followPaths(List(s), Set(finish), finish, next)).mkString).foreach(println)
    // FDABEGHICKLMJNOPQRTUVWXYZS not good :-(
    removeDups(followPaths(starts.diff(finishes).toList, Set(finish), finish, next)).mkString
}

  override def exampleAnswerPart2: String = ""
  override def solvePart2(lines: List[String]): String = {
    "[Not implemented]"
  }

}

object Day07 extends App {
  new Day07().solvePuzzles("/2018/day07.txt")
}
