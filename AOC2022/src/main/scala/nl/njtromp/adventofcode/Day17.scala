package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day17 extends Puzzle[Long] {
  private type Block = Set[(Int, Int)]
  private val leftWall = 0
  private val rightWall = 8
  // The left wall is at x=0 and the right wall is at x=8
  // Define shapes so that the lowest part is a y=0 and the left most at x=0
  private val minus = Set((1, 0), (2, 0), (3, 0), (4, 0))
  private val plus = Set((2, 2), (1, 1), (2, 1), (3, 1), (2, 0))
  private val hook = Set((3, 2), (3, 1), (1, 0), (2, 0), (3, 0))
  private val pipe = Set((1, 3), (1, 2), (1, 1), (1, 0))
  private val dot = Set((1, 1), (2, 1), (1, 0), (2, 0))
  // Move all shapes two units to the right
  private val blocks = Array(minus, plus, hook, pipe, dot).map(_.map(s => (s._1 + 2, s._2)))

  @tailrec
  private def simulateBlocksFalling(nrOfBlocks: Int, blockIndex: Int, jetIndex: Int, jets: Array[Char], cave: Set[(Int, Int)]): Set[(Int, Int)] =
    @tailrec
    def simulateBlock(block: Block, jetIndex: Int): (Block, Int) =
      val jettedBlock = jets(jetIndex) match {
        case '<' => if (block.minBy(_._1)._1 == leftWall + 1) block else block.map(b => (b._1 - 1, b._2))
        case '>' => if (block.maxBy(_._1)._1 == rightWall - 1) block else block.map(b => (b._1 + 1, b._2))
      }
      val blockToFall = if (jettedBlock.intersect(cave).isEmpty) jettedBlock else block
      val fallenBlock = blockToFall.map(b => (b._1, b._2 - 1))
      if (fallenBlock.intersect(cave).nonEmpty)
        (blockToFall, (jetIndex + 1) % jets.length)
      else
        simulateBlock(fallenBlock, (jetIndex + 1) % jets.length)

    if (nrOfBlocks == 0)
      cave
    else
      val startY = cave.maxBy(_._2)._2 + 4
      val newBlock = blocks(blockIndex).map(b => (b._1, b._2 + startY))
      val (block, newJet) = simulateBlock(newBlock, jetIndex)
      simulateBlocksFalling(nrOfBlocks - 1, (blockIndex +  1) % blocks.length, newJet, jets, cave ++ block)

  override def exampleAnswerPart1: Long = 3068
  override def solvePart1(lines: List[String]): Long = {
    val jets = lines.head.toArray
    val cave = simulateBlocksFalling(2022, 0, 0, jets, (1 to 7).map((_, 0)).toSet)
    cave.maxBy(_._2)._2
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    -1
  }

  private def printCave(cave: Set[(Int, Int)]): Unit =
    val maxY = cave.maxBy(_._2)._2
    (maxY to 0 by -1).foreach(y =>
      (1 to 7).foreach(x => print(if (cave.contains(x, y)) '#' else '.'))
      println()
    )
    println

}

object Day17 extends App{
  new Day17().solvePuzzles("/day17.txt")
}
