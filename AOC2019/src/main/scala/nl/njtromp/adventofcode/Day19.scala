package nl.njtromp.adventofcode

import scala.collection.mutable

class Day19 extends Puzzle[Long] {
  private type Pos = (Int, Int)
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

  private def execute(program: Array[Long], x: Int, y: Int): Boolean =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long].withDefaultValue(0L)
    var inputCounter = 0
    var inRange = false

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
    def input(ip: Int, mode: Int): Int =
      if inputCounter == 0 then
        write(mode, program(ip + 1), x)
      else
        write(mode, program(ip + 1), y)
      inputCounter += 1
      ip + 2
    def output(ip: Int, mode: Int): Int =
      val value = read(mode, program(ip + 1))
      inRange = value == 1
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
          ip = input(ip, opcode.toInt / 100)
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
    inRange

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.isEmpty then return 0
    val program = lines.head.split(",").map(_.toLong)
    (0 until 50).map(y =>
      (0 until 50).count(x => execute(program.clone(), x, y))
    ).sum

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.isEmpty then return 0
    val program = lines.head.split(",").map(_.toLong)
    var y = 10 // Just make sure we skip the empty space
    var x = 0
    val tileSize = 100
    var foundIt = false
    while !foundIt do
      // Find upper left corner
      while !execute(program.clone(), x, y) do
        x += 1
      // If the line is not wide enough then try the next line
      if !execute(program.clone(), x + tileSize - 1, y) then
        y += 1
      else
        // While we did not find the lower left and there is an upper right, try one position to the right
        var dx = 0
        while !execute(program.clone(), x + dx, y + tileSize - 1) && execute(program.clone(), x + dx + tileSize - 1, y) do
          dx += 1
        // Check if it fits
        if execute(program.clone(), x + dx, y + tileSize - 1) && execute(program.clone(), x + dx + tileSize - 1, y) then
          x += dx
          foundIt = true
        else
          y += 1
    x * 10000 + y
}

object Day19 extends App {
  new Day19().solvePuzzles()
}
