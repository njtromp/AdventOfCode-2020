package nl.njtromp.adventofcode

import scala.collection.mutable

class Day21 extends Puzzle[Long] {
  private val STOP = 99
  private val ADD = 1
  private val MUL = 2

  private val JUMP_IF_TRUE = 5
  private val JUMP_IF_FALSE = 6
  private val LESS_THEN = 7
  private val EQUALS = 8
  private val ADJUST_BP = 9

  private val INPUT = 3
  private val OUTPUT = 4

  private val POSITION = 0
  private val IMMEDIATE = 1
  private val RELATIVE = 2

  private var lastOutput: Long = 0
  private def execute(program: Array[Long]): Long =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long]

    def read(mode: Int, value: Long): Long =
      def readMappedMemory(address: Long): Long =
        if address < program.length then
          program(address.toInt)
        else
          extendedMemory(address)
      mode % 10 match
        case POSITION => readMappedMemory(value)
        case IMMEDIATE => value
        case RELATIVE => readMappedMemory(bp + value)
    def write(mode: Int, address: Long, value: Long): Unit =
      def writeMappedMemory(address: Long): Unit =
        if address < program.length then
          program(address.toInt) = value
        else
          extendedMemory(address) =  value
      mode % 10 match
        case IMMEDIATE => 
          val bla = read(mode, address)
          writeMappedMemory(bla)
        case RELATIVE => writeMappedMemory(bp + address)
        case POSITION => writeMappedMemory(address)

    def biFunction(ip: Int, mode: Int, f: (Long, Long) => Long): Int =
      write(mode / 100, program(ip + 3), f(read(mode, program(ip + 1)), read(mode / 10, program(ip + 2))))
      ip + 4
    def input(ip: Int, mode: Int, input: Long): Int =
      write(mode, program(ip + 1), input)
      ip + 2
    def output(ip: Int, mode: Int): Int =
      lastOutput = read(mode, program(ip + 1))
      if lastOutput == '\n' then
        println
      else
        print(lastOutput.toChar)
      ip + 2
    def jumpIfTrue(ip: Int, mode: Int): Int =
      if read(mode, program(ip + 1)) != 0 then
        read(mode / 10, program(ip + 2)).toInt
      else
        ip + 3
    def jumpIfFalse(ip: Int, mode: Int): Int =
      if read(mode, program(ip + 1)) == 0 then
        read(mode / 10, program(ip + 2)).toInt
      else
        ip + 3
    def lessThen(ip: Int, mode: Int): Int =
      write(mode / 100, program(ip + 3), if read(mode, program(ip + 1)) < read(mode / 10, program(ip + 2)) then 1 else 0)
      ip + 4
    def equals(ip: Int, mode: Int): Int =
      write(mode / 100, program(ip + 3), if read(mode, program(ip + 1)) == read(mode / 10, program(ip + 2)) then 1 else 0)
      ip + 4
    def adjustBp(ip: Int, mode: Int): Int =
      bp += read(mode, program(ip + 1)).toInt
      ip + 2

    var opcode: Long = program(ip)
    while opcode != STOP do
      opcode % 100 match
        case ADD =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a + b)
        case MUL =>
          ip = biFunction(ip, opcode.toInt / 100, (a, b) => a * b)
        case INPUT =>
          ip = input(ip, opcode.toInt / 100, instructions(inputCount))
          inputCount += 1
        case OUTPUT =>
          ip = output(ip, opcode.toInt / 100)
        case JUMP_IF_TRUE =>
          ip = jumpIfTrue(ip, opcode.toInt / 100)
        case JUMP_IF_FALSE =>
          ip = jumpIfFalse(ip, opcode.toInt / 100)
        case LESS_THEN =>
          ip = lessThen(ip, opcode.toInt / 100)
        case EQUALS =>
          ip = equals(ip, opcode.toInt / 100)
        case ADJUST_BP =>
          ip = adjustBp(ip, opcode.toInt / 100)
      opcode = program(ip)
    println
    lastOutput

  private var inputCount: Int = 0
  private var instructions = ""

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.isEmpty then return 0
    inputCount = 0
    instructions = List(
      // Jump if there is a hole anywhere before location D unless D is a hole itself or A is a hole
      "OR D J", // Only jump if D is not a hole
      "OR A T", // Can we walk?
      "AND B T", // Is there a hole at B ...
      "AND C T", // ... or at C ...
      "NOT T T", // Then jump
      "AND T J", // if there is a D
      "WALK"
    ).mkString("", "\n", "\n")
    execute(lines.head.split(",").map(_.toLong))

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.isEmpty then return 0
    inputCount = 0
    instructions = List(
      // Jump if there is a hole anywhere before location D unless D is a hole itself or A is a hole
      "OR A J",   // Can we walk?
      "AND B J",  // Is there a hole at B ...
      "AND C J",  // ... or at C ...
      "NOT J J",  // Then jump
      "AND D J",  // Only jump if D is not a hole ...
      // Up until now it is a simplefied version of part 1 that doesn't use register T
      "OR E T",   // ... and at least E ...
      "OR H T",   // ... or H ...
      "AND T J",  // ... is not a hole.
      "RUN"
    ).mkString("", "\n", "\n")
    execute(lines.head.split(",").map(_.toLong))

}

object Day21 extends App {
  new Day21().solvePuzzles()
}
