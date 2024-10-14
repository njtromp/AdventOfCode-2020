package nl.njtromp.adventofcode

import scala.collection.mutable

class Day07 extends Puzzle[Long] {
  private case class ProgramState(id: Char, ip: Int, in: mutable.Queue[Long], out: mutable.Queue[Long], program: Array[Long])

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

  private var lastOutput = 0L
  private def execute(tasks: mutable.Queue[ProgramState]): Long =
    var id = ' '
    var program = Array.empty[Long]
    var ip: Int = 0
    var in = mutable.Queue.empty[Long]
    var out = mutable.Queue.empty[Long]

    def read(mode: Int, value: Long): Long =
      if mode % 10 == IMMEDIATE then value else program(value.toInt)
    def biFunction(ip: Int, mode: Int, f: (Long, Long) => Long): Int =
      program(program(ip + 3).toInt) = f(read(mode, program(ip + 1)), read(mode / 10, program(ip + 2)))
      ip + 4
    def input(ip: Int, input: Long): Int =
      program(program(ip + 1).toInt) = input
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
      program(program(ip + 3).toInt) =
        if read(mode, program(ip + 1)) < read(mode / 10, program(ip + 2)) then
          1
        else
          0
      ip + 4
    def equals(ip: Int, mode: Int): Int =
      program(program(ip + 3).toInt) = if read(mode, program(ip + 1)) == read(mode / 10, program(ip + 2)) then 1 else 0
      ip + 4
    def saveTask(): Unit =
      tasks.enqueue(ProgramState(id, ip, in, out, program))
    def taskSwitch(): Unit =
      if tasks.nonEmpty then
        val task = tasks.dequeue()
        id = task.id
        ip = task.ip
        in = task.in
        out = task.out
        program = task.program

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
            ip = input(ip, in.dequeue())
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
        opcode = program(ip)
      taskSwitch()
    lastOutput

  private def runPrograms(programs: List[ProgramState]): Long =
    val tasks = mutable.Queue.empty[ProgramState]
    tasks.addAll(programs)
    execute(tasks)

  def optimizeOutput(lines: List[String], phaseSettings: Range): Long =
    lines.map(l =>
      val program = l.split(",").map(_.toLong)
      phaseSettings.permutations
        .map(state =>
          val qs = state.map(s => mutable.Queue[Long](s)).toList // Initialize each queue with the phase setting
          qs.head.enqueue(0) // and the first input for module A
          val inOuts: Iterator[List[mutable.Queue[Long]]] = (qs.head :: qs.reverse).reverse.sliding(2)
          val programs = ('A' to 'E').zip(inOuts).map(io => ProgramState(io._1, 0, io._2.head, io._2.last, program.clone())).toList
          runPrograms(programs)
        ).max
    ).sum

  // 1,0,4,3,2 ---------------------------------------------+
  // 0,1,2,3,4 ------------------------------------|        |
  // 4,3,2,1,0 -----------------------------V      V        V
  override def exampleAnswerPart1: Long = 43210 + 54321 + 65210
  override def solvePart1(lines: List[String]): Long =
    // In case of examples, only use the first 3
    optimizeOutput(lines.take(3), 0 to 4)

  // 9,7,8,5,6 -----------------------------------------+
  // 9,8,7,6,5 -------------------------------V         V
  override def exampleAnswerPart2: Long = 139629729 + 18216
  override def solvePart2(lines: List[String]): Long =
    if lines.size == 1 then
      optimizeOutput(lines, 5 to 9)
    else
      // Skip the part 1 examples
      optimizeOutput(lines.drop(3), 5 to 9)

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
