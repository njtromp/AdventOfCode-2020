package nl.njtromp.adventofcode

class Day02 extends Puzzle[Long] {
  private val STOP = 99
  private val ADD = 1
  private val MUL = 2

  private def runProgram(instructions: Array[Int]): Long =
    def add(ip: Int): Unit =
      val a1 = instructions(ip + 1)
      val a2 = instructions(ip + 2)
      val r = instructions(ip + 3)
      instructions(r) = instructions(a1) + instructions(a2)
    def mul(ip: Int): Unit =
      val a1 = instructions(ip + 1)
      val a2 = instructions(ip + 2)
      val r = instructions(ip + 3)
      instructions(r) = instructions(a1) * instructions(a2)
    var ip = 0
    while instructions(ip) != STOP do
      instructions(ip) match
        case ADD => add(ip)
        case MUL => mul(ip)
      ip += 4
    println(instructions.mkString(","))
    instructions.head

  override def exampleAnswerPart1: Long = 2 + 2 + 2 + 30
  override def solvePart1(lines: List[String]): Long =
    if lines.size == 1 then
      val instructions = lines.head.split(",").map(_.toInt)
      instructions(1) = 12
      instructions(2) = 2
      runProgram(instructions)
    else
      lines.map(l => runProgram(l.split(",").map(_.toInt))).sum

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day02 extends App {
  new Day02().solvePuzzles()
}
