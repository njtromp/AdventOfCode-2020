package nl.njtromp.adventofcode

import scala.collection.mutable

class Day17 extends Puzzle[Long] {
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

  private val deltas = List(
    (0, -1),
    (0, 1),
    (-1, 0),
    (1, 0),
  )

  private def countCrossings(scaffolds: mutable.Set[Pos]): Long =
    val minX = scaffolds.minBy(_._1)._1
    val maxX = scaffolds.maxBy(_._1)._1
    val minY = scaffolds.minBy(_._2)._2
    val maxY = scaffolds.maxBy(_._2)._2
    (minY + 1 until maxY).flatMap(y =>
      (minX + 1 until maxX)
        .filter(x => scaffolds.contains(x, y))
        .filter(x => deltas.forall(d => scaffolds.contains(x + d._1, y + d._2 )))
        .map(_ * y)
    ).sum

  private def execute(program: Array[Long]): Long =
    var ip: Int = 0
    var bp: Int = 0
    val extendedMemory = mutable.Map.empty[Long, Long]
    var x: Int = 0
    var y: Int = 0
    val scaffolds = mutable.Set.empty[Pos]

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
      val value = read(mode, program(ip + 1))
      if value == '\n' then
        println
        y += 1
        x = 0
      else
        print(value.toChar)
        if value == 35 then
          scaffolds += ((x, y))
        x += 1
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
          // Is not being used in the ASCII program so for easy of adopting the program lets use 0
          ip = input(ip, opcode.toInt / 100, 0)
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
    countCrossings(scaffolds)

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    if lines.isEmpty then return 0
    execute(lines.head.split(",").map(_.toLong))


  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day17 extends App {
  new Day17().solvePuzzles()
}
