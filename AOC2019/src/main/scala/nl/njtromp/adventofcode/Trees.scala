package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

abstract class Node
abstract class Leaf extends Node
abstract class NodeList(children: List[Node]) extends Node

trait Trees {
  def createLeaf(line: String): Leaf

  def createNodeList(children: List[Node]): NodeList

  def reconstructInOrder(line: String): Node = fromInOrder(line, '[', ']', ',')

  def fromInOrder(line: String, start: Char, finish: Char, separator: Char): Node =
    var stack = List.empty[mutable.ListBuffer[Node]]
    @tailrec
    def reconstruct(line: String): Node =
      if (line.head == start)
        stack = mutable.ListBuffer.empty[Node] :: stack
        reconstruct(line.tail)
      else if (line.head == finish)
        val node = createNodeList(stack.head.toList)
        stack = stack.tail
        if (stack.isEmpty)
          node
        else
         stack.head += node
         reconstruct(line.tail)
      else if (line.head == separator)
        reconstruct(line.tail)
      else
        val valueEnd = line.indexWhere(c => c == start || c == separator || c == finish)
        stack.head += createLeaf(line.take(valueEnd))
        reconstruct(line.drop(valueEnd))
    reconstruct(line)

}

