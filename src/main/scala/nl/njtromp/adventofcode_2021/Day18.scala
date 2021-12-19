package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

class Day18 extends Puzzle with RegexParsers {
  def leaf: Parser[Number] = "\\d+".r ^^ { n => Leaf(n.toLong)}
  def node: Parser[Number] = "[" ~> (leaf|node) ~ "," ~ (leaf|node) <~ "]" ^^ {
    case l ~ _ ~ r => Node(l, r)
  }

  sealed abstract class Number {
    def value: Long
    def lhs: Number = null
    def rhs: Number = null
    def isLeaf = false
    def canExplode = false
    def parent(child: Number): Option[Number]
    def asList(exploding: Number): List[Number]
    def replace(mapping: mutable.ArrayBuffer[(Number, Number, Int)]): Number
    def split: Number
    def explode: Number = explodeNested(this)
    def magnitude: Long
  }
  case class Leaf(value: Long) extends Number {
    override def isLeaf = true
    override def parent(child: Number): Option[Number] = None
    override def asList(exploding: Number): List[Number] = List(this)
    override def replace(mapping: mutable.ArrayBuffer[(Number, Number, Int)]): Number = {
      mapping.indices.foreach(i =>
        if (mapping(i)._1 == this)
          mapping(i) = (mapping(i)._1, mapping(i)._2, mapping(i)._3 - 1)
      )
      val replaceCandidates = mapping.filter(m => m._1 == this && m._3 == 0)
      if (replaceCandidates.size > 1)
        println("OEPSIE")
      replaceCandidates.headOption match {
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
    override def lhs: Number = left
    override def rhs: Number = right
    override def canExplode: Boolean = left.isLeaf && right.isLeaf
    override def parent(child: Number): Option[Number] = if (left == child || right == child)
      Some(this)
    else {
      left.parent(child) match {
        case None => right.parent(child)
        case Some(p) => Some(p)
      }
    }
    override def asList(exploding: Number): List[Number] =
      if (this == exploding) List(this) else left.asList(exploding) ++ right.asList(exploding)
    override def replace(mapping: mutable.ArrayBuffer[(Number, Number, Int)]): Number = {
      mapping.indices.foreach(i =>
        if (mapping(i)._1 == this)
          mapping(i) = (mapping(i)._1, mapping(i)._2, mapping(i)._3 - 1)
      )
      val replaceCandidates = mapping.filter(m => m._1 == this && m._3 == 0)
      if (replaceCandidates.size > 1)
        println("OEPSIE")
      replaceCandidates.headOption match {
        case None => Node(left.replace(mapping), right.replace(mapping))
        case Some(m) => m._2
      }
    }
    def split: Node = {
      val lSplitted = left.split
      if (lSplitted == left)
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
      val values = root.asList(exploding)
      // TODO Problem, the first node that matches the exploding node is not guaranteed to be the exploding node :-(
      val inFront = values.takeWhile(_ != exploding).reverse // In Reverse order for easy access the 'last' number
      val behind = values.dropWhile(_ != exploding)
      val mapping: mutable.ArrayBuffer[(Number, Number, Int)] = (inFront.take(1).headOption, behind.tail.take(1).headOption) match {
        case (None, Some(r)) => mutable.ArrayBuffer(
          (exploding, Leaf(0), 1),
          (r, Leaf(r.value + exploding.rhs.value), 1 + inFront.count(_ == r))
        )
        case (Some(l), None) => mutable.ArrayBuffer(
          (exploding, Leaf(0), 1),
          (l, Leaf(l.value + exploding.lhs.value), inFront.count(_ == l))
        )
        case (Some(l), Some(r)) => mutable.ArrayBuffer(
          (exploding, Leaf(0), 1),
          (l, Leaf(l.value + exploding.lhs.value), inFront.count(_ == l)),
          (r, Leaf(r.value + exploding.rhs.value), 1 + inFront.count(_ == r)))
      }
      root.replace(mapping)
    }
    toExplode(0, root) match {
      case None => root
      case Some(e) => explode(e)
    }
  }

  private def parseSnailfishNumber(snailfishNumber: String): Number = {
    println(s"Parsing: $snailfishNumber")
    parse(node, snailfishNumber) match {
      case Success(n, _) => n
    }
  }
  def reduce(root: Number): Number = {
    val exploded = root.explode
    if (exploded == root) {
      val splitted = root.split
      if (splitted == root) {
        println(s"Reduced: $root")
        root
      } else {
        println(s"Splitted: $splitted")
        reduce(splitted)
      }
    } else {
      println(s"Exploded: $exploded")
      reduce(exploded)
    }
  }


  override def solvePart1(lines: List[String]): Long = {
    val sum = reduce(lines.map(l => reduce(parseSnailfishNumber(l))).reduce((a, b) => {
      val toReduce = Node(a, b)
      println(s"Reducing $toReduce")
      val c = reduce(toReduce)
      println(s"Reduced to: $c")
      c
    }))
    println(sum)
    sum.magnitude
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day18 extends App {
  new Day18().solvePuzzles("/2021/day18.txt")
}
