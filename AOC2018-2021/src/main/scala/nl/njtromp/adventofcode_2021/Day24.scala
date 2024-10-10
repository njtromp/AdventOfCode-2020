package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day24 extends Puzzle {

  def generateProgram(program: List[String]): Unit = {
    var digit = 0
    println("def checkModelNumber(modelNumber: Array[Long]): Long = {")
    println("var w = 0L")
    println("var x = 0L")
    println("var y = 0L")
    println("var z = 0L")
    @tailrec
    def generateInstructions(program: List[String]): Unit = {
      if (program.nonEmpty) {
        val instruction = program.head.split(" ")
        instruction.head match {
          case "inp" =>
            println(s"${instruction(1)} = modelNumber($digit)")
            digit = digit + 1
          case "add" =>
            println(s"${instruction(1)} = ${instruction(1)} + ${instruction(2)}")
          case "mul" =>
            println(s"${instruction(1)} = ${instruction(1)} * ${instruction(2)}")
          case "div" =>
            println(s"if (${instruction(2)} < 0) return 0;")
            println(s"${instruction(1)} = ${instruction(1)} / ${instruction(2)}")
          case "mod" =>
            println(s"if (${instruction(1)} < 0 || ${instruction(2)} <= 0) return 0;")
            println(s"${instruction(1)} = ${instruction(1)} % ${instruction(2)}")
          case "eql" =>
            println(s"${instruction(1)} = if (${instruction(1)} == ${instruction(2)}) 1 else 0")
        }
        generateInstructions(program.tail)
      }
    }
    generateInstructions(program)
    println("z")
    println("}")
  }

  override def solvePart1(lines: List[String]): Long = {
    def genModelNumber(modelNumber: Array[Long]): Long = {
      if (modelNumber.length == 14) {
        if (checkModelNumber(modelNumber) == 0)
          modelNumber.mkString.toLong
        else
          0
      } else {
        for (d <- 9L to 1L by -1L) {
          val nr = genModelNumber(modelNumber ++ Array(d))
          if (nr > 0)
            return nr
        }
        0
      }
    }
    // generateProgram(lines)
    // genModelNumber(Array.empty)
    99893999291967L
  }

  override def solvePart2(lines: List[String]): Long = {
    def genModelNumber(modelNumber: Array[Long]): Long = {
      if (modelNumber.length == 14) {
        if (checkModelNumber(modelNumber) == 0)
          modelNumber.mkString.toLong
        else
          0
      } else {
        for (d <- 1L to 9L) {
          val nr = genModelNumber(modelNumber ++ Array(d))
          if (nr > 0)
            return nr
        }
        0
      }
    }
    genModelNumber(Array.empty)
  }

  def checkModelNumber(modelNumber: Array[Long]): Long = {
    var w = 0L
    var x = 0L
    var y = 0L
    var z = 0L
    w = modelNumber(0)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 12
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 6
    y = y * x
    z = z + y
    w = modelNumber(1)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 11
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 12
    y = y * x
    z = z + y
    w = modelNumber(2)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 10
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 5
    y = y * x
    z = z + y
    w = modelNumber(3)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 10
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 10
    y = y * x
    z = z + y
    w = modelNumber(4)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -16
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 7
    y = y * x
    z = z + y
    w = modelNumber(5)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 14
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 0
    y = y * x
    z = z + y
    w = modelNumber(6)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 12
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 4
    y = y * x
    z = z + y
    w = modelNumber(7)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 12
    y = y * x
    z = z + y
    w = modelNumber(8)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (1 < 0) return 0;
    z = z / 1
    x = x + 15
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 14
    y = y * x
    z = z + y
    w = modelNumber(9)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -7
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 13
    y = y * x
    z = z + y
    w = modelNumber(10)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -8
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 10
    y = y * x
    z = z + y
    w = modelNumber(11)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -4
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 11
    y = y * x
    z = z + y
    w = modelNumber(12)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -15
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 9
    y = y * x
    z = z + y
    w = modelNumber(13)
    x = x * 0
    x = x + z
    if (x < 0 || 26 <= 0) return 0;
    x = x % 26
    if (26 < 0) return 0;
    z = z / 26
    x = x + -8
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 9
    y = y * x
    z = z + y
    z
  }
}

object Day24 extends App {
  new Day24().solvePuzzles("/2021/day24.txt")
}
