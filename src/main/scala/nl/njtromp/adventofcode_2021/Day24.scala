package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day24 extends Puzzle {

  private val INP = raw"inp (.)".r
  private val ADD = raw"(add) (.) (.+)".r
  private val MUL = raw"(mul) (.) (.+)".r
  private val DIV = raw"(div) (.) (.+)".r
  private val MOD = raw"(mod) (.) (.+)".r
  private val EQL = raw"(eql) (.) (.+)".r

  def runProgram(modelNumber: String, w: Int, x: Int, y: Int, z: Int, program: List[String]): Boolean = {
    def getValue(v: String): Int = v match {
      case "w" => w
      case "x" => x
      case "y" => y
      case "z" => z
      case _ => v.toInt
    }
    def binaryOperation(op: String, r1: String, r2: String): Int = {
      op match {
        case "add" => getValue(r1) + getValue(r2)
        case "mul" => getValue(r1) * getValue(r2)
        case "div" => if (getValue(r2) == 0)
          0//throw new IllegalArgumentException(s"Invalid model-number part: $modelNumber.")
        else
          getValue(r1) / getValue(r2)
        case "mod" => if (getValue(r1) == 0 || getValue(r2) <= 0)
          0 //throw new IllegalArgumentException(s"Invalid model-number part: $modelNumber.")
        else
          getValue(r1) % getValue(r2)
        case "eql" => if (getValue(r1) == getValue(r2)) 1 else 0
      }
    }
    def binaryInstruction(op: String, r1: String, r2: String): Boolean = {
      r1 match {
        case "w" => runProgram(modelNumber, binaryOperation(op, r1, r2), x, y, z, program.tail)
        case "x" => runProgram(modelNumber, w, binaryOperation(op, r1, r2), y, z, program.tail)
        case "y" => runProgram(modelNumber, w, x, binaryOperation(op, r1, r2), z, program.tail)
        case "z" => runProgram(modelNumber, w, x, y, binaryOperation(op, r1, r2), program.tail)
      }
    }

    if (program.isEmpty)
      z == 0 // Valid model number
    else {
      program.head match {
        case INP(v) => v match {
          case "w" => runProgram(modelNumber.tail, modelNumber.head.asDigit, x, y, z, program.tail)
          case "x" => runProgram(modelNumber.tail, w, modelNumber.head.asDigit, y, z, program.tail)
          case "y" => runProgram(modelNumber.tail, w, x, modelNumber.head.asDigit, z, program.tail)
          case "z" => runProgram(modelNumber.tail, w, x, y, modelNumber.head.asDigit, program.tail)
        }
        case ADD(op, r1, r2) => binaryInstruction(op, r1, r2)
        case MUL(op, r1, r2) => binaryInstruction(op, r1, r2)
        case DIV(op, r1, r2) => binaryInstruction(op, r1, r2)
        case MOD(op, r1, r2) => binaryInstruction(op, r1, r2)
        case EQL(op, r1, r2) => binaryInstruction(op, r1, r2)
      }
    }
  }


  override def solvePart1(lines: List[String]): Long = {
    def genModelNumber(modelNumber: String): Long = {
      if (modelNumber.length == 14) {
        if (runProgram(modelNumber, 0, 0, 0, 0, lines))
          modelNumber.toLong
        else
          0
      } else {
        for (d <- 9 to 1 by -1) {
          val nr = genModelNumber(modelNumber + d)
          if (nr > 0)
            return nr
        }
        0
      }
    }
    genModelNumber("")
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day24 extends App {
  new Day24().solvePuzzles("/2021/day24.txt")
}
