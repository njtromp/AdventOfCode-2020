package nl.njtromp.adventofcode

import scala.collection.mutable

class Day11 extends Puzzle[Long] {
  private type Pos = (Int, Int)
  private val UP = 0
  private val RIGHT = 1
  private val DOWN = 2
  private val LEFT = 3

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

  private def execute(program: Array[Long], initialValue: Long): mutable.Map[Pos, Long] =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long].withDefaultValue(0L)
    val tiles = mutable.Map.empty[Pos, Long].withDefaultValue(initialValue)
    var robot: Pos = (0, 0)
    var facing = UP
    var paint = true

    def move(robot: Pos, rotate: Long): Pos =
      facing = (facing + (if rotate == 1 then 1 else -1) + 4) % 4
      val (x, y) = robot
      facing match
        case UP => (x, y - 1)
        case RIGHT => (x + 1, y)
        case DOWN => (x, y + 1)
        case LEFT => (x - 1, y)

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
      val input = tiles(robot)
      write(mode, program(ip + 1), input)
      ip + 2
    def output(ip: Int, mode: Int): Int =
      val output = read(mode, program(ip + 1))
      if paint then
        tiles(robot) = output
      else
        robot = move(robot, output)
      paint = !paint
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
    tiles

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.nonEmpty then
      val program = lines.head.split(",").map(_.toLong)
      val tiles = execute(program, 0)
      tiles.keys.size
    else
      0

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.nonEmpty then
      val program = lines.head.split(",").map(_.toLong)
      val tiles = execute(program, 1)
      val minX = tiles.keys.map(_._1).min + 1 // Remove some random noise
      val maxX = tiles.keys.map(_._1).max - 3 // Remove some random noise
      val minY = tiles.keys.map(_._2).min
      val maxY = tiles.keys.map(_._2).max
      (minY to maxY).foreach(y =>
        (minX to maxX).foreach(x => print(if tiles(x, y) == 1 then 'X' else ' '))
        println
      )
      println("AHCHZEPK")
    0

}

object Day11 extends App {
  new Day11().solvePuzzles()
}
