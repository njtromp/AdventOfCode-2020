package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day08 extends App {
  var instructions: List[String] = List.empty
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle08.txt")).getLines) {
    instructions = line +: instructions
  }
  instructions = instructions.reverse

  val NOP = "nop [+-]?\\d+".r
  val JMP = "jmp ([+-]?\\d+)".r
  val ACC = "acc ([+-]?\\d+)".r

  var usedInstructions: Set[Int] = Set.empty
  var acc = 0
  var ip = 0
  while (!usedInstructions.contains(ip)) {
    usedInstructions += ip
    instructions(ip) match {
      case NOP() => ip += 1
      case JMP(deltaIp) => ip += deltaIp.toInt
      case ACC(deltaAcc) => {
        acc += deltaAcc.toInt
        ip += 1
      }
    }
  }
  println(s"Answer part 1: $acc")

  val answerPart2 = -1
  println(s"Answer part 2: $answerPart2")

}
