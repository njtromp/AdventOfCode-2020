package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day21 extends Puzzle[Long] {
  private val ROOT = "root"
  private val HUMAN = "humn"
  private val monkey = "(\\w{4}): (.+)".r
  private val number = "(\\d+)".r
  private val operation = "(\\w{4}) (=|\\+|-|\\*|/) (\\w{4})".r

  private class Monkey(val name: String, val action: String) {
    def shout(monkeys: Map[String, Monkey]): Long =
      action match {
        case number(number) => number.toLong
        case operation(monkeyLeft, operation, monkeyRight) => operation match {
          case "+" => monkeys(monkeyLeft).shout(monkeys) + monkeys(monkeyRight).shout(monkeys)
          case "-" => monkeys(monkeyLeft).shout(monkeys) - monkeys(monkeyRight).shout(monkeys)
          case "*" => monkeys(monkeyLeft).shout(monkeys) * monkeys(monkeyRight).shout(monkeys)
          case "/" => monkeys(monkeyLeft).shout(monkeys) / monkeys(monkeyRight).shout(monkeys)
        }
      }
    def solve(monkeys: Map[String, Monkey]): String =
      action match {
        case number(number) => number
        case operation(monkeyLeft, operation, monkeyRight) =>
          val solvedLeft = monkeys(monkeyLeft).solve(monkeys)
          val solvedRight = monkeys(monkeyRight).solve(monkeys)
          (solvedLeft, solvedRight) match {
            case (number(_), number(_)) => shout(monkeys).toString
            case (_, _) =>
              operation match {
                case "+" => s"($solvedLeft + $solvedRight)"
                case "-" => s"($solvedLeft - $solvedRight)"
                case "*" => s"($solvedLeft * $solvedRight)"
                case "/" => s"($solvedLeft / $solvedRight)"
              }
          }
      }
  }

  private case class Human(override val name: String, override val action: String) extends Monkey(name, action) {
    override def shout(monkeys: Map[String, Monkey]): Long = action.toLong
    override def solve(monkeys: Map[String, Monkey]): String = name
  }

  private case class Root(override val name: String, override val action: String) extends Monkey(name, action) {
    override def shout(monkeys: Map[String, Monkey]): Long =
      println(s"${monkeys(action.substring(0, 4)).shout(monkeys)} == ${monkeys(action.substring(7)).shout(monkeys)}")
      -1
    override def solve(monkeys: Map[String, Monkey]): String =
      @tailrec
      def solve(monkey: Monkey, answer: Long): String =
        monkey match {
          case Human(_, _) => answer.toString
          case _ => monkey.action match {
            case operation(monkeyLeft, operation, monkeyRight) =>
              operation match {
              case "+" => if (monkeys(monkeyLeft).solve(monkeys).matches("\\d+"))
                solve(monkeys(monkeyRight), answer - monkeys(monkeyLeft).shout(monkeys))
              else
                solve(monkeys(monkeyLeft), answer - monkeys(monkeyRight).shout(monkeys))
              case "-" => if (monkeys(monkeyLeft).solve(monkeys).matches("\\d+"))
                solve(monkeys(monkeyRight), -(answer - monkeys(monkeyLeft).shout(monkeys)))
              else
                solve(monkeys(monkeyLeft), answer + monkeys(monkeyRight).shout(monkeys))
              case "*" => if (monkeys(monkeyLeft).solve(monkeys).matches("\\d+"))
                solve(monkeys(monkeyRight), answer / monkeys(monkeyLeft).shout(monkeys))
              else
                solve(monkeys(monkeyLeft), answer / monkeys(monkeyRight).shout(monkeys))
              case "/" => if (monkeys(monkeyLeft).solve(monkeys).matches("\\d+"))
                solve(monkeys(monkeyRight), monkeys(monkeyLeft).shout(monkeys) / answer)
              else
                solve(monkeys(monkeyLeft), answer * monkeys(monkeyRight).shout(monkeys))
            }
          }
        }
      action match {
        case operation(monkeyLeft, _, monkeyRight) =>
          solve(monkeys(monkeyLeft), monkeys(monkeyRight).shout(monkeys))
      }
  }

  override def exampleAnswerPart1: Long = 152
  override def solvePart1(lines: List[String]): Long =
    val monkeys = lines.map({
      case monkey(name, action) => Monkey(name, action)
    }).map(m => m.name -> m).toMap
    monkeys(ROOT).shout(monkeys)

  override def exampleAnswerPart2: Long = 301
  override def solvePart2(lines: List[String]): Long = {
    val originalMonkeys = lines.map({
      case monkey(name, action) => Monkey(name, action)
    }).map(m => m.name -> m).toMap
    // It turns out that the right side of the equation results in a number and that the left size contains 'humn'.
    // So we can make use of this knowledge within Root.solve
    val monkeys = originalMonkeys + (ROOT -> Root(ROOT, originalMonkeys(ROOT).action)) + (HUMAN -> Human(HUMAN, ""))
    val answer = monkeys(ROOT).solve(monkeys)
//    println(monkeys("root").shout(monkeys + ("humn" -> Human("humn", answer))))
    answer.toLong
  }

}

object Day21 extends App{
  new Day21().solvePuzzles("/day21.txt")
}
