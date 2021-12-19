package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

class Day18 extends Puzzle with RegexParsers {
  def leaf: Parser[Number] = "\\d+".r ^^ { n => Leaf(n.toLong)}
  def node: Parser[Number] = "[" ~> (leaf|node) ~ "," ~ (leaf|node) <~ "]" ^^ {
    case l ~ _ ~ r => Node(l, r)
  }

  // Don't be alarmed when the compiler warns about matches not being exhaustive on lines 92, 95, 104 and 127.
  // The possible cases the compiler 'sees' can't occur in real life.
  sealed abstract class Number {
    def value: Long
    def isLeaf = false
    def canExplode = false
    def parent(child: Number): Option[Number]
    def asList(exploding: Number): List[Number]
    def replace(mapping: mutable.ArrayBuffer[(Number, Number)]): Number
    def split: Number
    def explode: Number = explodeNested(this)
    def magnitude: Long
  }
  case class Leaf(value: Long) extends Number {
    override def isLeaf = true
    override def parent(child: Number): Option[Number] = None
    override def asList(exploding: Number): List[Number] = List(this)
    override def replace(mapping: mutable.ArrayBuffer[(Number, Number)]): Number = {
      mapping.find(m => m._1.eq(this)) match {
        case None => this
        case Some(m) => m._2
      }
    }
    def split: Number = if (value > 9)
      Node(Leaf(value / 2), Leaf(value - (value / 2)))
    else
      this
    def magnitude: Long = value
    override def toString: String = value.toString
  }
  case class Node(left: Number, right: Number) extends Number {
    // A Node should never be asked for its value (for now), If it happens it indicates a major problem in the algorithm.
    override def value: Long = ???
    override def canExplode: Boolean = left.isLeaf && right.isLeaf
    override def parent(child: Number): Option[Number] = if (left.eq(child) || right.eq(child))
      Some(this)
    else {
      left.parent(child) match {
        case None => right.parent(child)
        case Some(p) => Some(p)
      }
    }
    override def asList(exploding: Number): List[Number] =
      if (this.eq(exploding)) List(this) else left.asList(exploding) ++ right.asList(exploding)
    override def replace(mapping: mutable.ArrayBuffer[(Number, Number)]): Number = {
      mapping.find(m => m._1.eq(this)) match {
        case None => Node(left.replace(mapping), right.replace(mapping))
        case Some(m) => m._2
      }
    }
    def split: Node = {
      val lSplitted = left.split
      if (lSplitted.equals(left))
        Node(left, right.split)
      else
        Node(lSplitted, right)
    }

    def magnitude: Long = 3 * left.magnitude + 2 * right.magnitude
    override def toString: String = s"[${left.toString},${right.toString}]"
  }

  private def explodeNested(root: Number): Number = {
    def toExplode(level: Int, node: Number): Option[Number] = {
      if (node.canExplode && level >= 4) {
        Some(node)
      } else {
        node match {
          case _: Leaf => None
          case Node(l, r) => toExplode(level + 1, l) match {
            case None => toExplode(level + 1, r)
            case Some(e) => Some(e)
          }
        }
      }
    }
    def explode(exploding: Number): Number = {
      def leftValue(node: Number): Long = node match {
        case Node(l, _) => l.value
      }
      def rightValue(node: Number): Long = node match {
        case Node(_, r) => r.value
      }
      // Get all the leafs and the exploding node in a list. The order is in pre-order. The Leaf
      // that can participate in the explosion is in front of the exploding node while the Leaf on the right
      // is the first one behind the exploding node.
      val values = root.asList(exploding)
      val inFront = values.takeWhile(!_.eq(exploding)).reverse // In Reverse order for easy access the right-most number
      val behind = values.dropWhile(!_.eq(exploding))
      val mapping: mutable.ArrayBuffer[(Number, Number)] = (inFront.take(1).headOption, behind.tail.take(1).headOption) match {
        case (None, Some(r)) => mutable.ArrayBuffer(
          (exploding, Leaf(0)),
          (r, Leaf(r.value + rightValue(exploding)))
        )
        case (Some(l), None) => mutable.ArrayBuffer(
          (exploding, Leaf(0)),
          (l, Leaf(l.value + leftValue(exploding)))
        )
        case (Some(l), Some(r)) => mutable.ArrayBuffer(
          (exploding, Leaf(0)),
          (l, Leaf(l.value + leftValue(exploding))),
          (r, Leaf(r.value + rightValue(exploding))))
      }
      root.replace(mapping)
    }
    toExplode(0, root) match {
      case None => root
      case Some(e) => explode(e)
    }
  }

  private def parseSnailfishNumber(snailfishNumber: String): Number = {
    parse(node, snailfishNumber) match {
      case Success(n, _) => n
    }
  }

  @tailrec
  private def reduce(root: Number): Number = {
    val exploded = root.explode
    if (exploded.equals(root)) {
      val splitted = root.split
      if (splitted.equals(root)) {
        root
      } else {
        reduce(splitted)
      }
    } else {
      reduce(exploded)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val sum = reduce(lines.map(l => reduce(parseSnailfishNumber(l))).reduce((a, b) => {
      reduce(Node(a, b))
    }))
    sum.magnitude
  }

  override def solvePart2(lines: List[String]): Long = {
    lines.flatMap(l1 => lines.filterNot(_ == l1).map(l2 => List(l1, l2))).foldLeft(0L)((acc, ls) => Math.max(acc, solvePart1(ls)))
  }
}

object Day18 extends App {
  new Day18().solvePuzzles("/2021/day18.txt")
}
