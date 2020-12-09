package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day08 extends App {
  var instructions: Array[String] = Array.empty
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle08.txt")).getLines) {
    instructions = line +: instructions
  }
  instructions = instructions.reverse

  val NOP = "nop ([+-]?\\d+)".r
  val JMP = "jmp ([+-]?\\d+)".r
  val ACC = "acc ([+-]?\\d+)".r

  var usedInstructions: Set[Int] = Set.empty
  var acc = 0
  var ip = 0
  simulate
  println(s"Answer part 1: $acc")

  fixBug
  println(s"Answer part 2: $acc")

  private def simulate = {
    while (!usedInstructions.contains(ip) && ip < instructions.size) {
      usedInstructions += ip
      instructions(ip) match {
        case NOP(_) => ip += 1
        case JMP(deltaIp) => ip += deltaIp.toInt
        case ACC(deltaAcc) => {
          acc += deltaAcc.toInt
          ip += 1
        }
      }
    }
  }

  private def fixBug: Int = {
    var bugIp = 0
    ip = 0
    while (ip != instructions.size) {
      while (instructions(bugIp).startsWith("acc ")) {
        bugIp += 1
      }
      val originalInstruction = instructions(bugIp)
      originalInstruction match {
        case NOP(counter) => instructions(bugIp) = s"jmp ${counter}"
        case JMP(counter) => instructions(bugIp) = s"nop ${counter}"
      }
      ip = 0
      acc = 0
      usedInstructions = Set.empty
      simulate
      if (ip == instructions.size) {
        return acc
      }
      instructions(bugIp) = originalInstruction
      bugIp += 1
    }
    -1
  }
}
