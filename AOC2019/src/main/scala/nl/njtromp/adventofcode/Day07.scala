package nl.njtromp.adventofcode

class Day07 extends Puzzle[Long] {
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

  // TODO change program to array of Long
  private def execute(program: Array[Int], phaseSetting: Int, inputValue: Int): Long =
    var outputValue: Long = 0
    def read(mode: Int, value: Int): Int =
      if mode % 10 == IMMEDIATE then value else program(value)
    def biFunction(ip: Int, mode: Int, f: (Int, Int) => Int): Int =
      program(program(ip + 3)) = f(read(mode, program(ip + 1)), read(mode / 10, program(ip + 2)))
      ip + 4
    def input(ip: Int, input: Int): Int =
      program(program(ip + 1)) = input
      ip + 2
    def output(ip: Int, mode: Int): Int =
      if outputValue != 0 then
        println("Multiple outputs!!!!!")
      outputValue = read(mode, program(ip + 1))
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
    var inputCount = -1
    while opcode != STOP do
      opcode % 100 match
        case ADD =>
          ip = biFunction(ip, opcode / 100, (a, b) => a + b)
        case MUL =>
          ip = biFunction(ip, opcode / 100, (a, b) => a * b)
        case INPUT =>
          inputCount += 1
          ip = input(ip, if inputCount == 0 then phaseSetting else inputValue)
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
    outputValue

  private def runProgram(program: Array[Int], phaseSettings: IndexedSeq[Int]): Int =
    var input = 0
    phaseSettings.foreach(phaseSetting => input = execute(program.clone, phaseSetting, input).toInt)
    input

  def runPrograms(lines: List[String]): Long =
    lines.map(l => {
      val program = l.split(",").map(_.toInt)
      (0 to 4).permutations.map(settings =>
        runProgram(program, settings)
      ).max
    }).sum

  // 4,3,2,1,0
  // 0,1,2,3,4
  // 1,0,4,3,2
  override def exampleAnswerPart1: Long = 43210 + 54321 + 65210
  override def solvePart1(lines: List[String]): Long =
    runPrograms(lines)

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
