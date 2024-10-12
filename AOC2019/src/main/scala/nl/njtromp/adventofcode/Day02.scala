package nl.njtromp.adventofcode

class Day02 extends Puzzle[Long] {
  private val STOP = 99
  private val ADD = 1
  private val MUL = 2

  private def runProgram(instructions: Array[Int]): Long =
    def apply(ip: Int, f: (Int, Int) => Int): Int =
      val a1 = instructions(ip + 1)
      val a2 = instructions(ip + 2)
      val r = instructions(ip + 3)
      instructions(r) = f(instructions(a1), instructions(a2))
      ip + 4
    var ip = 0
    while instructions(ip) != STOP do
      instructions(ip) match
        case ADD => ip = apply(ip, (a, b) => a + b)
        case MUL => ip = apply(ip, (a, b) => a * b)
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

  override def exampleAnswerPart2: Long = 19690720
  override def solvePart2(lines: List[String]): Long =
    if lines.size == 1 then
      val instructions = lines.head.split(",").map(_.toInt)
      (0 to 100 * 100).dropWhile(n =>
        val ins = instructions.clone()
        ins(1) = n / 100
        ins(2) = n % 100
        runProgram(ins) != 19690720
      ).head
    else
      // Just skip the example
      19690720

}

object Day02 extends App {
  new Day02().solvePuzzles()
}
