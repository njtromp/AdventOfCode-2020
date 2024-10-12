package nl.njtromp.adventofcode

class Day05 extends Puzzle[Long] {
  private val STOP = 99
  private val ADD = 1
  private val MUL = 2
  private val INPUT = 3
  private val OUTPUT = 4
  private val POSITION = 0
  private val IMMEDIATE = 1
  private val JUMP_IF_TRUE = 5
  private val JUMP_IF_FALSE = 6
  private val LESS_THEN = 7
  private val EQUALS = 8

  private def runProgram(program: Array[Int], systemId: Int): Long =
    var printedValue: Long = 0
    def read(mode: Int, value: Int): Int =
      if mode % 10 == IMMEDIATE then value else program(value)
    def biFunction(ip: Int, mode: Int, f: (Int, Int) => Int): Int =
      program(program(ip + 3)) = f(read(mode, program(ip + 1)), read(mode / 10, program(ip + 2)))
      ip + 4
    def input(ip: Int, mode: Int, input: Int): Int =
      program(program(ip + 1)) = input
      ip + 2
    def output(ip: Int, mode: Int): Int =
      printedValue = read(mode, program(ip + 1))
      ip + 2
    def jumpIfTrue(ip: Int, mode: Int): Int =
      if read(mode, program(ip + 1)) != 0 then
        read(mode / 10, program(ip + 2))
      else
        ip + 3
    def jumpIfFalse(ip: Int, mode: Int): Int =
      if read(mode, program(ip + 1)) == 0 then
        read(mode / 10, program(ip + 2))
      else
        ip + 3
    def lessThen(ip: Int, mode: Int): Int =
      program(program(ip + 3)) =
        if read(mode, program(ip + 1)) < read(mode / 10, program(ip + 2)) then
          1
        else
          0
      ip + 4
    def equals(ip: Int, mode: Int): Int =
      program(program(ip + 3)) = if read(mode, program(ip + 1)) == read(mode / 10, program(ip + 2)) then 1 else 0
      ip + 4

    var ip = 0
    var opcode = program(ip)
    while opcode != STOP do
      opcode % 100 match
        case ADD =>
          ip = biFunction(ip, opcode / 100, (a, b) => a + b)
        case MUL =>
          ip = biFunction(ip, opcode / 100, (a, b) => a * b)
        case INPUT =>
          ip = input(ip, opcode / 100, systemId)
        case OUTPUT =>
          ip = output(ip, opcode / 100)
        case JUMP_IF_TRUE =>
          ip = jumpIfTrue(ip, opcode / 100)
        case JUMP_IF_FALSE =>
          ip = jumpIfFalse(ip, opcode / 100)
        case LESS_THEN =>
          ip = lessThen(ip, opcode / 100)
        case EQUALS =>
          ip = equals(ip, opcode / 100)
      opcode = program(ip)
    printedValue

  def runPrograms(lines: List[String], systemId: Int): Long =
    if lines.size == 1 then
      val instructions = lines.head.split(",").map(_.toInt)
      runProgram(instructions, systemId)
    else
      lines.map(l => runProgram(l.split(",").map(_.toInt), systemId)).sum

  override def exampleAnswerPart1: Long = -1
  override def solvePart1(lines: List[String]): Long =
    runPrograms(lines, 1)

  override def exampleAnswerPart2: Long = -1
  override def solvePart2(lines: List[String]): Long =
    runPrograms(lines, 5)

}

object Day05 extends App {
  new Day05().solvePuzzles()
}
