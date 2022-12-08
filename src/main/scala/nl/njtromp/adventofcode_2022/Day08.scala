package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day08 extends Puzzle2 {

  override def exampleAnswerPart1: Long = 21

  def edges(trees: Array[Array[Int]]): Int =
    2 * trees.length + 2 * (trees(0).length - 2)

  def tallestLeft(trees: Array[Array[Int]], x: Int, y: Int): Int =
    trees(y).take(x).max
  def tallestRightt(trees: Array[Array[Int]], x: Int, y: Int): Int =
    trees(y).drop(x + 1).max
  def tallestUp(trees: Array[Array[Int]], x: Int, y: Int): Int =
    (0 until y).foldLeft(0)((a, t) => Math.max(a, trees(t)(x)))
  def tallestDown(trees: Array[Array[Int]], x: Int, y: Int): Int =
    (y + 1 until trees(0).length).foldLeft(0)((a, t) => Math.max(a, trees(t)(x)))

  def isVisible(trees: Array[Array[Int]], x: Int, y: Int): Boolean = {
    tallestLeft(trees, x, y) < trees(y)(x) ||
    tallestRightt(trees, x, y) < trees(y)(x) ||
    tallestUp(trees, x, y) < trees(y)(x) ||
    tallestDown(trees, x, y) < trees(y)(x)
  }

  def visibleTrees(trees: Array[Array[Int]]): Array[Int] = {
    val heights = (1 until trees.length - 1).flatMap(y =>
      (1 until trees(0).length - 1).map(x => {
        if (isVisible(trees, x, y)) trees(x)(y) else -1
      })
    ).toArray
    heights.filter(_ >= 0)
  }

  override def solvePart1(lines: List[String]): Long = {
    val trees = lines.map(_.map(_.asDigit).toArray).toArray
    edges(trees) + visibleTrees(trees).length
  }

  override def exampleAnswerPart2: Long = 8

  def numberVisibleTreesLeft(trees: Array[Array[Int]], x: Int, y: Int): Int = {
    var t = x - 1
    while (t >= 0) {
      if (trees(y)(t) >= trees(y)(x)) return x - t;
      t -= 1
    }
    x
  }

  def numberVisibleTreesRight(trees: Array[Array[Int]], x: Int, y: Int): Int = {
    var t = x + 1
    while (t < trees(0).length) {
      if (trees(y)(t) >= trees(y)(x)) return t - x;
      t += 1
    }
    trees(0).length - 1 - x
  }

  def numberVisibleTreesUp(trees: Array[Array[Int]], x: Int, y: Int): Int = {
    var t = y - 1
    while (t >= 0) {
      if (trees(t)(x) >= trees(y)(x)) return y - t;
      t -= 1
    }
    y
  }

  def numberVisibleTreesDown(trees: Array[Array[Int]], x: Int, y: Int): Int = {
    var t = y + 1
    while (t < trees.length) {
      if (trees(t)(x) >= trees(y)(x)) return t - y;
      t += 1
    }
    trees.length - 1- y
  }

  def scenicScore(trees: Array[Array[Int]], x: Int, y: Int): Long = {
    val treesLeft = numberVisibleTreesLeft(trees, x, y)
    val treesRight = numberVisibleTreesRight(trees, x, y)
    val treedUp = numberVisibleTreesUp(trees, x, y)
    val treesDown = numberVisibleTreesDown(trees, x, y)
    treesLeft * treesRight * treedUp * treesDown
  }

  override def solvePart2(lines: List[String]): Long = {
    val trees = lines.map(_.map(_.asDigit).toArray).toArray
    val scores = trees.indices.flatMap(y => trees(y).indices.map(x => scenicScore(trees, x, y)))
    scores.max
  }

}

object Day08 extends App {
  new Day08().solvePuzzles("/2022/day08.txt")
}
