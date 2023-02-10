package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day16 extends Puzzle2 {

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

  private val BEFORE = "Before: \\[(.*)]".r
  private val AFTER = "After:  \\[(.*)]".r

  private def matchOpcodes(sample: List[String]): (Int, List[Int]) = {
    val before = sample.head match {
      case BEFORE(regs) => regs.split(",").map(_.trim.toLong)
    }
    val instruction = sample(1).split(" ").map(_.toInt)
    val expected = sample(2) match {
      case AFTER(regs) => regs.split(",").map(_.trim.toLong)
    }
    val possibleOpcodes = opcodes.indices.filter(o => {
      val regs = before.clone()
      opcodes(o)(instruction(1), instruction(2), instruction(3), regs)
      regs sameElements expected
    }).toList
    (instruction.head, possibleOpcodes)
  }

  private def runProgram(regs: Array[Long], instructions: List[Array[Int]], opcodeMapping: Array[Int]): Unit = {
    instructions.foreach(i => opcodes(opcodeMapping(i(0)))(i(1), i(2), i(3), regs))
  }

  @tailrec
  private def deduceMapping(samples: List[(Int, List[Int])], mapping: List[(Int, Int)]): Array[Int] = {
    if (samples.isEmpty) {
      mapping.sortBy(_._1).map(_._2).toArray
    } else {
      val knownMappings = samples.filter(_._2.length == 1).map(m => (m._1, m._2.head)).distinct
      val newMappings = knownMappings.map(_._2).toSet
      deduceMapping(samples.map(m => (m._1, m._2.filterNot(newMappings.contains))).filter(_._2.nonEmpty), knownMappings ++ mapping)
    }
  }

  override def exampleAnswerPart1: Long = 1
  override def solvePart1(lines: List[String]): Long = {
    val samples = lines.sliding(4, 4).takeWhile(_.head.nonEmpty)
    samples.map(matchOpcodes).count(_._2.size >= 3)
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    val samples = lines.sliding(4, 4).takeWhile(_.head.nonEmpty).toList
    val instructions = lines.drop(samples.length * 4).filterNot(_.isEmpty).map(_.split(" ").map(_.toInt))
    val opcodesPerSample = samples.map(matchOpcodes)
    if (opcodesPerSample.length > 1) {
      val opcodeMapping: Array[Int] = deduceMapping(opcodesPerSample, List.empty[(Int, Int)])
      val regs = Array.fill[Long](4)(0L)
      runProgram(regs, instructions, opcodeMapping)
      regs(0)
    } else
      0
  }

}

object Day16 extends App {
  new Day16().solvePuzzles("/2018/day16.txt")
}
