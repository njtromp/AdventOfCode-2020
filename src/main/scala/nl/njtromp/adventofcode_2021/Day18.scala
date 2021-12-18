package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.util.parsing.combinator.RegexParsers

class Day18 extends Puzzle with RegexParsers {
  def leaf: Parser[SnailFishNumber] = "\\d+".r ^^ { n => Leaf(n.toLong)}
  def node: Parser[SnailFishNumber] = "[" ~> (leaf|node) ~ "," ~ (leaf|node) <~ "]" ^^ {
    case l ~ _ ~ r => Node(l, r)
  }

  sealed abstract class SnailFishNumber {
    def split: SnailFishNumber
    def explode: SnailFishNumber
    def reduce: SnailFishNumber = {
      val exploded = this.explode
      if (exploded.equals(this)) {
        val splitted = this.split
        if (splitted.equals(this))
          this
        else
          splitted.reduce
      } else
        exploded.reduce
    }
    def magnitude: Long
  }
  case class Leaf(value: Long) extends SnailFishNumber {
    def split: SnailFishNumber = if (value > 9)
      Node(Leaf(value / 2), Leaf(value - (value / 2)))
    else {
      this
    }
    def explode: SnailFishNumber = this
    def magnitude: Long = value
    override def toString: String = value.toString
  }
  case class Node(left: SnailFishNumber, right: SnailFishNumber) extends SnailFishNumber {
    def split: Node = Node(left.split, right.split)
    def explode: SnailFishNumber = {
      // Tricky part!
      this
    }
    def magnitude: Long = 3 * left.magnitude + 2 * right.magnitude
    override def toString: String = s"[${left.toString},${right.toString}]"
  }

  private def parseSnailfishNumber(snailfishNumber: String): SnailFishNumber = {
    parse(node, snailfishNumber) match {
      case Success(n, _) => n
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val sum = lines.map(parseSnailfishNumber).reduce((a, b) => Node(a, b).reduce)
    println(sum)
    sum.magnitude
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day18 extends App {
  new Day18().solvePuzzles("/2021/day18.txt")
}
