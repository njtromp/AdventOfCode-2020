package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

class Day08 extends Puzzle2 {

  private case class Node(children: Array[Node]) {
    var metadata: Array[Int] = Array.empty[Int]
    def sumMetadata(): Int = metadata.sum + children.map(_.sumMetadata()).sum
    def size(): Int = 2 + metadata.length + children.map(_.size()).sum
    def value(): Int = {
      if (children.length == 0)
        sumMetadata()
      else {
        metadata.filter(_ <= children.length).map(c => children(c - 1).value()).sum
      }
    }
  }

  private def createNodes(numbers: List[Int]): Node = {
    numbers match {
      case childCount :: metaSize :: tail =>
        val node = Node(Array.ofDim[Node](childCount))
        for (c <- 0 until childCount) {
          node.children(c) = createNodes(tail.drop(node.children.take(c).map(_.size()).sum))
        }
        val meta = tail.drop(node.children.map(_.size()).sum)
        node.metadata = meta.take(metaSize).toArray
        node
    }
  }

  override def exampleAnswerPart1: Long = 138
  override def solvePart1(lines: List[String]): Long = {
    val root = createNodes(lines.head.split(" ").map(_.toInt).toList)
    root.sumMetadata()
  }

  override def exampleAnswerPart2: Long = 66
  override def solvePart2(lines: List[String]): Long = {
    val root = createNodes(lines.head.split(" ").map(_.toInt).toList)
    root.value()
  }

}

object Day08 extends App {
  new Day08().solvePuzzles("/2018/day08.txt")
}
