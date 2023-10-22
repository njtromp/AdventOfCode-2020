package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec
import scala.collection.mutable

class Day19 extends Puzzle2 {

  private val addr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) + regs(b)
  private val addi = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) + b
  private val mulr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) * regs(b)
  private val muli = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) * b
  private val banr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) & regs(b)
  private val bani = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) & b
  private val borr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) | regs(b)
  private val bori = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a) | b
  private val setr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = regs(a)
  private val seti = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = a
  private val gtir = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = if (a > regs(b)) 1 else 0
  private val gtri = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = if (regs(a) > b) 1 else 0
  private val gtrr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = if (regs(a) > regs(b)) 1 else 0
  private val eqir = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = if (a == regs(b)) 1 else 0
  private val eqri = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = if (regs(a) == b) 1 else 0
  private val eqrr = (a: Int, b: Int, c: Int, regs: Array[Long]) => regs(c) = if (regs(a) == regs(b)) 1 else 0
  private val opcodes = Array(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)
  private val opcodeMapping = Array("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr")

//  private val ipUsageCount = mutable.Map.empty[Int, Long].withDefaultValue(0L)
  private def runProgram(ipReg: Int, regs: Array[Long], instructions: Array[(Int, Int, Int, Int)]): Long = {
    @tailrec
    def runProgram(ip: Long): Long = {
      if (ip >= 0 && ip < instructions.length) {
        val instr = instructions(ip.toInt)
        regs(ipReg) = ip
//        ipUsageCount(ip.toInt) = ipUsageCount(ip.toInt) + 1
//        print(f"$ip%2s - ${opcodeMapping(instr._1)}, ${instr._2} ${instr._3} ${instr._4} [${regs.mkString(" ")}]")
        opcodes(instr._1)(instr._2, instr._3, instr._4, regs)
//        println(s" => [${regs.mkString(" ")}]")
        runProgram(regs(ipReg) + 1L)
      } else {
        regs(0)
      }
    }
    runProgram(0L)
  }

  override def exampleAnswerPart1: Long = 6
  override def solvePart1(lines: List[String]): Long = {
    val ipReg = lines.head.charAt(4).asDigit
    val instructions = lines.drop(1).filterNot(_.isEmpty).map(_.split(" ")).map(l => (opcodeMapping.indexOf(l.head), l(1).toInt, l(2).toInt, l(3).toInt)).toArray
    runProgram(ipReg, Array.fill[Long](6)(0L), instructions)
  }

  override def exampleAnswerPart2: Long = 6
  override def solvePart2(lines: List[String]): Long = {
    val ipReg = lines.head.charAt(4).asDigit
    val instructions = lines.drop(1).filterNot(_.isEmpty).map(_.split(" ")).map(l => (opcodeMapping.indexOf(l.head), l(1).toInt, l(2).toInt, l(3).toInt)).toArray
    runProgram(ipReg, Array(1L, 0L, 0L, 0L, 0L, 0L), instructions)
  }

}

object Day19 extends App {
  new Day19().solvePuzzles("/2018/day19.txt")
}
