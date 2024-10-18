package nl.njtromp.adventofcode

import scala.collection.mutable

class Day13 extends Puzzle[Long] {
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

  var ballPosition = 0L
  var batPosition = 0L
  var score = 0L
  private def printScreen(instructions: List[Long]): Unit =
    val screen: Map[(Long, Long), Long] = instructions.grouped(3).map(g => ((g.head, g(1)), g.last)).toMap.withDefaultValue(0)
    val minX = screen.keys.foldLeft(Long.MaxValue)((a, i) => Math.min(a, i._1))
    val maxX = screen.keys.foldLeft(Long.MinValue)((a, i) => Math.max(a, i._1))
    val minY = screen.keys.foldLeft(Long.MaxValue)((a, i) => Math.min(a, i._2))
    val maxY = screen.keys.foldLeft(Long.MinValue)((a, i) => Math.max(a, i._2))
    def decode(x: Long, y: Long): Char = screen(x, y) match
      case 0 => ' '
      case 1 => '#'
      case 2 => 'x'
      case 3 =>
        batPosition = x
        '-'
      case 4 =>
        ballPosition = x
        'o'
    (minY to maxY).foreach(y =>
      (minX to maxX).foreach(x =>
        if (x, y) == (-1L, 0L) then
          score = screen(x, y)
//          print(s" $score\n ")
        else
//          print(decode(x, y))
          decode(x, y) // Force updating the locations of the bat and the ball
      )
//      println
    )

  private def execute(program: Array[Long]): mutable.Queue[Long] =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long].withDefaultValue(0L)
    val instructions = mutable.Queue.empty[Long]

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
      printScreen(instructions.toList)
      val input = Math.signum(ballPosition - batPosition).toLong
      write(mode, program(ip + 1), input)
      ip + 2
    def output(ip: Int, mode: Int): Int =
      val output = read(mode, program(ip + 1))
      instructions.enqueue(output)
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
    instructions

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.nonEmpty then
      val program = lines.head.split(",").map(_.toLong)
      execute(program).grouped(3).map(_.last).count(_ == 2)
    else
      0

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.nonEmpty then
      val program = lines.head.split(",").map(_.toLong)
      program(0) = 2
      printScreen(execute(program).toList)
      score
    else
      0

}

object Day13 extends App {
  new Day13().solvePuzzles()
}
