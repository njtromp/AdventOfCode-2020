package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day11 extends Puzzle2 {
  private val MonkeyID  = "Monkey (\\d+):".r
  private val Items     = "Starting items: (.+)".r
  private val Plus      = "Operation: new = old \\+ (\\d+)".r
  private val PlusOld   = "Operation: new = old \\+ old".r
  private val Minus     = "Operation: new = old - (\\d+)".r
  private val Times     = "Operation: new = old \\* (\\d+)".r
  private val TimesOld  = "Operation: new = old \\* old".r
  private val Divide    = "Operation: new = old / (\\d+)".r
  private val Test      = "Test: divisible by (\\d+)".r
  private val True      = "If true: throw to monkey (\\d+)".r
  private val False     = "If false: throw to monkey (\\d+)".r

  abstract class Operation {
    def apply(old: Long): Long
  }
  case class MinusOp(value: Long) extends Operation {
    override def apply(old: Long): Long = old - value
  }
  case class PlusOp(value: Long) extends Operation {
    override def apply(old: Long): Long = old + value
  }
  case class TimesOp(value: Long) extends Operation {
    override def apply(old: Long): Long = old * value
  }
  case class TimesOldOp() extends Operation {
    override def apply(old: Long): Long = old * old
  }
  case class DivideOp(value: Long) extends Operation {
    override def apply(old: Long): Long = old / value
  }

  def parseItems(line: String): Array[Long] = {
    line match {
      case Items(itemList) => itemList.split(",").map(_.trim.toLong)
    }
  }

  def parseOperation(op: String): Operation = {
    op match {
      case Plus(value) => PlusOp(value.toInt)
      case PlusOld() => TimesOp(2)
      case Minus(value) => MinusOp(value.toInt)
      case Times(value) => TimesOp(value.toInt)
      case TimesOld() => TimesOldOp()
      case Divide(value) => DivideOp(value.toInt)
    }
  }

  def parseId(line: String): Int =
    line match {
      case MonkeyID(id) => id.toInt
    }

  def parseTest(line: String): Int =
    line match {
      case Test(id) => id.toInt
    }

  def parseTrue(line: String): Int =
    line match {
      case True(id) => id.toInt
    }

  def parseFalse(line: String): Int =
    line match {
      case False(id) => id.toInt
    }

  def createMonkey(lines: List[String]): Monkey = {
    val id = parseId(lines.head.trim)
    val items = parseItems(lines(1).trim)
    val op = parseOperation(lines(2).trim)
    val testValue = parseTest(lines(3).trim)
    val trueID = parseTrue(lines(4).trim)
    val falseId = parseFalse(lines(5).trim)
    val monkey = Monkey(id, op, testValue, trueID, falseId)
    items.foreach(monkey.append)
    monkey
  }

  def parseMonkeys(lines: List[String]): List[Monkey] = {
    if (lines.isEmpty)
      Nil
    else {
      val monkeyLines = lines.takeWhile(_.nonEmpty)
      createMonkey(monkeyLines) :: parseMonkeys(lines.drop(monkeyLines.size + 1))
    }
  }

  case class Monkey(id: Int, op: Operation, test: Int, monkeyTrue: Int, monkeyFalse: Int) {
    var inspections: Long = 0
    val items: Array[Long] = new Array[Long](40) // In puzzle input there are 36 items at max, so 40 should do the trick
    var numberOfItems: Int = 0

    def append(item: Long): Unit = {
      items(numberOfItems) = item
      numberOfItems += 1
    }

    def clearItems(): Unit = numberOfItems = 0

    def play(monkeys: Array[Monkey]): Unit = {
      for (i <- 0 until numberOfItems) {
        inspections += 1
        val newValue = (op.apply(items(i)) / Monkey.worryDivider) % Monkey.limiter
        if (newValue % test == 0)
          monkeys(monkeyTrue).append(newValue)
        else
          monkeys(monkeyFalse).append(newValue)
      }
      clearItems()
    }
  }
  object Monkey {
    var worryDivider: Long = 0
    var limiter: Long = 0
  }

  @tailrec
  private def playRound(monkeys: Array[Monkey], round: Int): Unit =
    if (round != 0) {
      monkeys.foreach(_.play(monkeys))
      playRound(monkeys, round - 1)
    }

  override def exampleAnswerPart1: Long = 10605L
  override def solvePart1(lines: List[String]): Long = {
    val monkeys = parseMonkeys(lines).toArray
    Monkey.worryDivider = 3
    Monkey.limiter = monkeys.map(_.test).product
    playRound(monkeys, 20)
    monkeys.sortBy(_.inspections).reverse.take(2).map(_.inspections).product
  }

  override def exampleAnswerPart2: Long = 2713310158L
  override def solvePart2(lines: List[String]): Long = {
    val monkeys = parseMonkeys(lines).toArray
    Monkey.worryDivider = 1
    Monkey.limiter = monkeys.map(_.test).product
    playRound(monkeys, 10000)
    monkeys.sortBy(_.inspections).reverse.take(2).map(_.inspections).product
  }

}

object Day11 extends App{
  new Day11().solvePuzzles("/2022/day11.txt")
}
