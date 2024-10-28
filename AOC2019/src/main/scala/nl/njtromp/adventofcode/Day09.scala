package nl.njtromp.adventofcode

import scala.collection.mutable

class Day09 extends Puzzle[Long] {
  private case class ProgramState(id: Char, ip: Int, bp: Int, in: mutable.Queue[Long], out: mutable.Queue[Long], extendedMemory: mutable.Map[Long, Long], program: Array[Long])

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


  private def execute(tasks: mutable.Queue[ProgramState]): Long =
    var lastOutput = 0L
    var id = ' '
    var program = Array.empty[Long]
    var ip: Int = 0
    var bp: Int = 0
    var in = mutable.Queue.empty[Long]
    var out = mutable.Queue.empty[Long]
    var extendedMemory = mutable.Map.empty[Long, Long]

    def saveTask(): Unit =
      tasks.enqueue(ProgramState(id, ip, bp, in, out, extendedMemory, program))
    def taskSwitch(): Unit =
      if tasks.nonEmpty then
        val task = tasks.dequeue()
        id = task.id
        ip = task.ip
        bp = task.bp
        in = task.in
        out = task.out
        extendedMemory = task.extendedMemory
        program = task.program

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
      out.enqueue(lastOutput)
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

    while tasks.nonEmpty do
      taskSwitch()
      var opcode: Long = program(ip)
      while opcode != STOP do
        opcode % 100 match
          case ADD =>
            ip = biFunction(ip, opcode.toInt / 100, (a, b) => a + b)
          case MUL =>
            ip = biFunction(ip, opcode.toInt / 100, (a, b) => a * b)
          case INPUT =>
            ip = input(ip, opcode.toInt / 100, in.dequeue())
          case OUTPUT =>
            ip = output(ip, opcode.toInt / 100)
            saveTask()
            taskSwitch()
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
      taskSwitch()
    lastOutput

  private def runPrograms(programs: List[ProgramState]): Long =
    val tasks = mutable.Queue.empty[ProgramState]
    tasks.addAll(programs)
    execute(tasks)

  override def exampleAnswerPart1: Long = 2344970539239587L
  override def solvePart1(lines: List[String]): Long =
    lines.map(l =>
      val program = l.split(",").map(_.toLong)
      val q = mutable.Queue.empty[Long]
      q.enqueue(1)
      runPrograms(List(ProgramState('A', 0, 0, q, q, mutable.Map.empty[Long, Long].withDefaultValue(0L), program)))
    ).sum


  override def exampleAnswerPart2: Long = 2344970539239587L
  override def solvePart2(lines: List[String]): Long =
    lines.map(l =>
      val program = l.split(",").map(_.toLong)
      val q = mutable.Queue.empty[Long]
      q.enqueue(2)
      runPrograms(List(ProgramState('A', 0, 0, q, q, mutable.Map.empty[Long, Long].withDefaultValue(0L), program)))
    ).sum

}

object Day09 extends App {
  new Day09().solvePuzzles()
}
