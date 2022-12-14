package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.util.parsing.combinator.RegexParsers

class Day13 extends Puzzle2 with RegexParsers {
  sealed abstract class Node {
    def leftMostValue: Int
  }
  case class NodeNumber(value: Int) extends Node {
    override def leftMostValue: Int = value
  }
  case class NodeList(nodes: List[Node]) extends Node {
    override def leftMostValue: Int = if (nodes.isEmpty) 0 else nodes.head.leftMostValue
  }
  case class Pair(id: Int, left: Node, right: Node) {
    def isValid: Option[Boolean] = (left, right) match {
      case (NodeNumber(l), NodeNumber(r)) =>
        if (l < r) Some(true) else if (l > r) Some(false) else None
      case (NodeNumber(l), NodeList(rs)) =>
        Pair(0, NodeList(List(left)), NodeList(rs)).isValid
      case (NodeList(ls), NodeNumber(r)) =>
        Pair(0, NodeList(ls), NodeList(List(right))).isValid
      case (NodeList(ls), NodeList(rs)) =>
        ls.zip(rs).foldLeft(Option.empty[Boolean])((a, e) => a match {
          case Some(f) => Some(f)
          case None => Pair(0, e._1, e._2).isValid
        }) match {
          case Some(f) => Some(f)
          case None => if (ls.size == rs.size) None else Some(ls.size < rs.size)
        }
    }
  }

  def number: Parser[NodeNumber] = """\d+""".r ^^ {n => NodeNumber(n.toInt)}
  def list: Parser[NodeList] = "[" ~> rep((number | list) | "," ~> (number | list)) <~ "]" ^^ (list => NodeList(list))

  override def exampleAnswerPart1: Long = 13
  override def solvePart1(lines: List[String]): Long = {
    val pairs = lines
      .filterNot(_.isEmpty)
      .map(parseAll(list, _).get)
      .sliding(2, 2)
      .zipWithIndex
      .map(p => Pair(p._2 + 1, p._1.head, p._1.last))
      .toList
    val validPairs = pairs.filter(p => p.isValid.get)
    validPairs.map(_.id).sum
  }

  override def exampleAnswerPart2: Long = 140
  override def solvePart2(lines: List[String]): Long = {
    val packets = lines
      .filterNot(_.isEmpty)
      .map(parseAll(list, _).get)
    val marker2 = parseAll(list, "[[2]]").get
    val marker6 = parseAll(list, "[[6]]").get
    val sortedPackets = (marker2 :: marker6 :: packets).sortBy(_.leftMostValue)
    (sortedPackets.takeWhile(_ != marker2).size + 1) * (sortedPackets.takeWhile(_ != marker6).size + 1)
  }

}

object Day13 extends App{
  new Day13().solvePuzzles("/2022/day13.txt")
}
