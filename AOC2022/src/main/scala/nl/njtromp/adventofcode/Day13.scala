package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day13 extends Puzzle[Long] with Trees {
  case class PacketValue(value: Int) extends Leaf {
    override def toString: String = value.toString
  }
  case class PacketList(children: List[Node]) extends NodeList(children) {
    override def toString: String = children.map(_.toString).mkString("[", ",", "]")
  }
  override def createLeaf(line: String): Leaf = PacketValue(line.toInt)
  override def createNodeList(children: List[Node]): NodeList = PacketList(children)

  private def correctOrder(packets: (Node, Node)): Option[Boolean] =
    packets match
      case (PacketValue(left), PacketValue(right)) =>
        if (left < right) Some(true) else if (left > right) Some(false) else None
      case (left: PacketValue, right: PacketList) =>
        correctOrder(PacketList(List(left)), right)
      case (left: PacketList, right: PacketValue) =>
        correctOrder(left, PacketList(List(right)))
      case (PacketList(left), PacketList(right)) =>
        left.zip(right).foldLeft(Option.empty[Boolean])((a, lr) => a match {
          case r: Some[Boolean] => r
          case _ => correctOrder(lr._1, lr._2)
        }) match {
          case r: Some[Boolean] => r
          case _ => if (left.size == right.size) Option.empty[Boolean] else Some(left.size < right.size)
        }

  override def exampleAnswerPart1: Long = 13
  override def solvePart1(lines: List[String]): Long = {
    val packetGroups = groupByEmptyLine(lines).map(_.map(reconstructInOrder)).map(ps => (ps.head, ps.last))
    packetGroups.zipWithIndex.filter(p => correctOrder(p._1).get).map(_._2 + 1).sum
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    -1
  }

}

object Day13 extends App{
  new Day13().solvePuzzles("/day13.txt")
}
