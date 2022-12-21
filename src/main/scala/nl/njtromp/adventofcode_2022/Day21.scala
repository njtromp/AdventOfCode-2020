package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day21 extends Puzzle2 {
  private val MATH_MONKEY = """([a-z]{4}): ([a-z]{4}) (\+|-|\*|/) ([a-z]{4})""".r
  private val VALUE_MONKEY = """([a-z]{4}): (-?\d+)""".r
  private val NUMBER = "-?\\d+"
  private val ROOT = "root"
  private val HUMAN = "humn"

  abstract class Monkey {
    def yell(): Long
    def solve(): String
  }
  case class MathMonkey(left: String, op: Char, right: String) extends Monkey {
    override def yell(): Long = {
      val leftValue = Monkey.monkeys(left).yell()
      val rightValue = Monkey.monkeys(right).yell()
      op match {
        case '+' => leftValue + rightValue
        case '-' => leftValue - rightValue
        case '*' => leftValue * rightValue
        case '/' => leftValue / rightValue
      }
    }
    override def solve(): String = {
      val leftValue = Monkey.monkeys(left).solve()
      val rightValue = Monkey.monkeys(right).solve()
      if (leftValue.matches(NUMBER) && rightValue.matches(NUMBER))
        yell().toString
      else
        s"($leftValue $op $rightValue)"
    }
  }
  case class ValueMonkey(value: Long) extends Monkey {
    override def yell(): Long = value
    override def solve(): String = value.toString
  }
  case class Human() extends Monkey {
    override def yell(): Long = ???
    override def solve(): String = HUMAN
  }
  object Monkey {
    var monkeys: Map[String, Monkey] = Map()
  }

  private def loadMonkeys(lines: List[String]): Map[String, Monkey] =
    lines.map({
      case VALUE_MONKEY(name, value) => name -> ValueMonkey(value.toLong)
      case MATH_MONKEY(name, left, op, right) => name -> MathMonkey(left, op.head, right)
    }).toMap

  def solve(root: MathMonkey): Long = {
    @tailrec
    def solve(left: Monkey, answer: Long): Long = left match {
      case Human() => answer
      case MathMonkey(left, op, right) =>
        val leftValue = Monkey.monkeys(left)
        val rightValue = Monkey.monkeys(right)
        op match {
          case '+' => if (leftValue.solve().matches(NUMBER))
            solve(rightValue, answer - leftValue.yell())
          else
            solve(leftValue, answer - rightValue.yell())
          case '-' => if (leftValue.solve().matches(NUMBER))
            solve(rightValue, leftValue.yell() - answer)
          else
            solve(leftValue, answer + rightValue.yell())
          case '*' => if (leftValue.solve().matches(NUMBER))
            solve(rightValue, answer / leftValue.yell())
          else
            solve(leftValue, answer / rightValue.yell())
          case '/' => if (leftValue.solve().matches(NUMBER))
            solve(rightValue, leftValue.yell() / answer)
          else
            solve(leftValue, answer * rightValue.yell())
        }
    }
    val left = Monkey.monkeys(root.left)
    val right = Monkey.monkeys(root.right)
    println(s"${left.solve()} = ${right.solve()}")
    // Taking a shortcut, right side of the root results in a number
    // and the left size contains "humn" so whole solution is build on this knowledge
    solve(left, right.solve().toLong)
  }

  override def exampleAnswerPart1: Long = 152
  override def solvePart1(lines: List[String]): Long = {
    Monkey.monkeys = loadMonkeys(lines)
    Monkey.monkeys("root").yell()
  }

  override def exampleAnswerPart2: Long = 301
  override def solvePart2(lines: List[String]): Long = {
    Monkey.monkeys = loadMonkeys(lines) + (HUMAN -> Human())
    Monkey.monkeys(ROOT) match {
      case m: MathMonkey => solve(m)
    }
  }

}

object Day21 extends App{
  new Day21().solvePuzzles("/2022/day21.txt")
}
