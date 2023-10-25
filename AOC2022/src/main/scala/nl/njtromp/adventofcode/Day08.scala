package nl.njtromp.adventofcode

import scala.collection.mutable

class Day08 extends Puzzle[Long] {

  override def exampleAnswerPart1: Long = 21
  override def solvePart1(lines: List[String]): Long = {
    val map = SimpleMap[Int](lines, l => l.toCharArray.map(_.asDigit))
    val highTrees = mutable.ListBuffer.empty[(Int, Int)]
    def inspectTree(tree: (Int, Int), highestTree: Int): Int =
      if (map(tree) > highestTree) {
        highTrees += tree
        map(tree)
      } else
        highestTree
    // Rows from left to right
    (0 until map.height).foreach(row => {
      var highestTree = -1
      (0 until map.width).foreach(column => highestTree = inspectTree((row, column), highestTree))
    })
    // Rows from right to left
    (0 until map.height).foreach(row => {
      var highestTree = -1
      (map.width - 1 to 0 by -1).foreach(column => highestTree = inspectTree((row, column), highestTree))
    })
    // Columns from top to bottom
    (0 until map.width).foreach(column => {
      var highestTree = -1
      (0 until map.height).foreach(row => highestTree = inspectTree((row, column), highestTree))
    })
    // Columns from bottom to top
    (0 until map.width).foreach(column => {
      var highestTree = -1
      (map.height - 1 to 0 by -1).foreach(row => highestTree = inspectTree((row, column), highestTree))
    })
    // Count unique trees
    highTrees.toSet.size
  }

  override def exampleAnswerPart2: Long = 8
  override def solvePart2(lines: List[String]): Long = {
    val map = SimpleMap[Int](lines, l => l.toCharArray.map(_.asDigit))
    def scenicScore(tree: (Int, Int)): Int = {
      val up = map.column(tree._2).take(tree._1 + 1).reverse
      val toLeft = map.row(tree._1).take(tree._2 + 1).reverse
      val down = map.column(tree._2).drop(tree._1)
      val toRight = map.row(tree._1).drop(tree._2)

      val upSize = up.tail.takeUntil(_ >= up.head).size
      val leftSize = toLeft.tail.takeUntil(_ >= toLeft.head).size
      val downSize = down.tail.takeUntil(_ >= down.head).size
      val rightSize = toRight.tail.takeUntil(_ >= toRight.head).size

      rightSize * leftSize * downSize * upSize
    }
    (1 until map.height - 1).flatMap(row => (1 until map.width - 1).map(column => {
      val score = scenicScore((row, column))
      score
    })).max
  }

}

object Day08 extends App{
  new Day08().solvePuzzles("/day08.txt")
}
