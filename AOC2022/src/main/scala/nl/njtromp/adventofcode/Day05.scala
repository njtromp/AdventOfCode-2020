package nl.njtromp.adventofcode

import scala.collection.mutable

class Day05 extends Puzzle[String] {
  private def createStacks(lines: List[String]): Array[mutable.Stack[Char]] =
    val stacksInfo = lines.takeWhile(_.nonEmpty).reverse
    val stacks = stacksInfo.head.split("   ").map(_ => mutable.Stack[Char]())
    stacksInfo.tail.foreach(line => {
      (1 until stacks.length + 1).map(s => 1 + 4 * (s - 1))
        .filter(s => s < line.length && line.charAt(s) != ' ')
        .foreach(s => stacks((s - 1) / 4).push(line.charAt(s)))
    })
    stacks

  private def process(lines: List[String], craneOperator: (Array[mutable.Stack[Char]], Int, Int, Int) => Unit): Array[mutable.Stack[Char]] =
    val stacks = createStacks(lines)
    val move = "move (\\d+) from (\\d+) to (\\d+)".r
    lines.dropWhile(_.nonEmpty).drop(1)
      .foreach({case move(count, from, to) => craneOperator(stacks, count.toInt, from.toInt, to.toInt)})
    stacks

  override def exampleAnswerPart1: String = "[CMZ]"
  override def solvePart1(lines: List[String]): String = {
    def craneOperatorOne(stacks: Array[mutable.Stack[Char]], count: Int, from: Int, to: Int): Unit =
      (1 to count).foreach(_ => stacks(to - 1).push(stacks(from - 1).pop))

    val stacks = process(lines, craneOperatorOne)
    s"[${stacks.map(_.head).mkString}]"
  }

  override def exampleAnswerPart2: String = "[MCD]"
  override def solvePart2(lines: List[String]): String = {
    def craneOperatorTwo(stacks: Array[mutable.Stack[Char]], count: Int, from: Int, to: Int): Unit =
      val temp = mutable.Stack.empty[Char]
      (1 to count).foreach(_ => temp.push(stacks(from - 1).pop))
      (1 to count).foreach(_ => stacks(to - 1).push(temp.pop))

    val stacks = process(lines, craneOperatorTwo)
    s"[${stacks.map(_.head).mkString}]"
  }

}

object Day05 extends App{
  new Day05().solvePuzzles("/day05.txt")
}
