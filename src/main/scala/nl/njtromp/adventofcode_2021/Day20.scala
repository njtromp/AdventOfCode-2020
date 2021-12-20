package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day20 extends Puzzle {
  type Pos = (Int, Int)
  private val BORDER = 101; // For part 1 5 is sufficient (5 = 2 * steps + 1, 102 = 2 * steps + 1)
  private val SIZE = 100 + 2 * BORDER

  @tailrec
  private def enhance(steps: Int, image: Set[Pos], enhancements: Set[Int]): Set[Pos] = {
    val deltas = List(
      (-1, -1), (0, -1), (1, -1),
      (-1, 0), (0, 0), (1, 0),
      (-1, 1), (0, 1), (1, 1)
    )
    def onMap(n: Int): Int = if (n < 0) n + SIZE else if (n > (SIZE - 1)) n - SIZE else n
    def isLit(p: (Int, Int)): Boolean = {
      val neighbors = deltas.map(n => (p._1 + n._1, p._2 + n._2)).map(p => (onMap(p._1), onMap(p._2)))
      val binary = neighbors.map(p => if (image.contains(p)) "1" else "0").mkString
      val index = Integer.parseInt(binary, 2)
      enhancements.contains(index)
    }
    if (steps == 0)
      image
    else {
      val enhanced = (0 until SIZE).flatMap(y => (0 until SIZE).map(x => (x, y))).toList.filter(isLit).toSet
      enhance(steps - 1, enhanced, enhancements)
    }
  }

  def readImageData(lines: List[String]): (Set[(Int, Int)], Set[Int]) = {
    val enhancements = lines.head.zipWithIndex.filter(_._1 == '#').map(_._2).toSet
    val image: Set[Pos] = lines.tail.filterNot(_.isBlank).zipWithIndex
      .flatMap(row => row._1.zipWithIndex
        .filter(_._1 == '#')
        .map(d => (d._2 + BORDER, row._2 + BORDER))
      )
      .toSet
    (image, enhancements)
  }

  override def solvePart1(lines: List[String]): Long = {
    val (image, enhancement) = readImageData(lines)
    enhance(2, image, enhancement).size
  }

  override def solvePart2(lines: List[String]): Long = {
    val (image, enhancement) = readImageData(lines)
    enhance(50, image, enhancement).size
  }
}

object Day20 extends App {
  new Day20().solvePuzzles("/2021/day20.txt")
}
