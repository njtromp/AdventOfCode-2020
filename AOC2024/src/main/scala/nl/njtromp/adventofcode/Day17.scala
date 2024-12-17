package nl.njtromp.adventofcode

import scala.collection.mutable

class Day17 extends Puzzle[String] {

  private val A = 'A'
  private val B = 'B'
  private val C = 'C'
  private val ADV = 0
  private val BXL = 1
  private val BST = 2
  private val JNZ = 3
  private val BXC = 4
  private val OUT = 5
  private val BDV = 6
  private val CDV = 7

  private def parseReg(line: String): (Char, Long) =
    line match
      case s"Register $r: $v" => (r.head, v.toLong)

  private def runProgram(aStart: Long, bStart: Long, cStart: Long, program: Array[Long]): String =
    var a = aStart
    var b = bStart
    var c = cStart
    var pc = 0
    val output = mutable.Queue.empty[Long]
    def combo(op: Long): Long =
      op match
        case 4 => a
        case 5 => b
        case 6 => c
        case _ => op
    while pc < program.length do
      val op = program(pc + 1)
      program(pc) match
        case ADV => a /= (1 << combo(op))
        case BXL => b ^= op
        case BST => b = combo(op) % 8
        case JNZ =>
          if a != 0 then
            pc = op.toInt - 2
        case BXC => b ^= c
        case OUT => output.enqueue(combo(op) % 8)
        case BDV => b = a / (1 << combo(op))
        case CDV => c = a / (1 << combo(op))
      pc += 2
    output.mkString(",")

  override def exampleAnswerPart1: String = "4,6,3,5,6,3,5,2,1,0"
  override def solvePart1(lines: List[String]): String =
    val input = lines.take(5)
    val regs = groupByEmptyLine(input).head.map(parseReg).toMap
    val ins = groupByEmptyLine(input).last.head.drop(9).split(",").map(_.toLong)
    runProgram(regs(A), regs(B), regs(C), ins)

  override def exampleAnswerPart2: String = "117440"
  override def solvePart2(lines: List[String]): String =
    val input = if lines.length > 5 then lines.drop(5) else lines
    val regs = groupByEmptyLine(input).head.map(parseReg).toMap
    val program = groupByEmptyLine(input).last.head.drop(9)
    val ins = program.split(",").map(_.toLong)
    // Let's build up the program in reverse order
    // Reverse engineering (decompiling) the program showed that register A is divided by 8 each time the program loops.
    var skip = program.length - 1
    var a = 0L
    var result = runProgram(a, regs(B), regs(C), ins)
    while result != program do
      // If we have a partial solution
      if result == program.drop(skip) then
        a *= 8    // Compensate for the division by 8
        skip -= 2 // Add two more characters (1 digit and a comma) to the expected output
      else
        a += 1
      result = runProgram(a, regs(B), regs(C), ins)
    s"$a"
}

object Day17 extends App {
  new Day17().solvePuzzles()
}
