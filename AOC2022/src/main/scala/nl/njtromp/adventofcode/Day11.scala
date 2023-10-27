package nl.njtromp.adventofcode

import scala.collection.mutable

class Day11 extends Puzzle[Long] {
  private val monkey ="Monkey (\\d+):".r
  private val items = "  Starting items: (.+)".r
  private val timesOld = "  Operation: new = old \\* old".r
  private val timesValue = "  Operation: new = old \\* (\\d+)".r
  private val plusValue = "  Operation: new = old \\+ (\\d+)".r
  private val divisibleBy = "  Test: divisible by (\\d+)".r
  private val ifTrue = "    If true: throw to monkey (\\d+)".r
  private val ifFalse = "    If false: throw to monkey (\\d+)".r

  abstract class Operation {
    def apply(old: Long): Long
  }

  case class PlusValue(value: Long) extends Operation {
    override def apply(old: Long): Long = old + value
  }

  case class TimesValue(value: Long) extends Operation {
    override def apply(old: Long): Long = old * value
  }

  case class TimesOld() extends Operation {
    override def apply(old: Long): Long = old * old
  }

  private case class Monkey(id: Int, items: mutable.Buffer[Long], op: Operation, divider: Long, trueId: Long, falseId: Long) {
    def round(): List[Long] =
      items.indices.foreach(i => items(i) = (op.apply(items(i)) / Monkey.safety) % Monkey.limiter)
      items.indices.map(l => if (items(l) % divider == 0L) trueId else falseId).toList
  }

  object Monkey {
    var safety: Long = 3L
    var limiter: Long = 0L
  }

  private def createMonkey(lines: List[String]): Monkey =
    val id = lines.head match {case monkey(id) => id.toInt}
    val numbers = lines(1) match {case items(is) => is.split(", ").map(_.toLong).toBuffer[Long]}
    val op: Operation = lines(2) match {
      case timesOld() => TimesOld()
      case timesValue(number) => TimesValue(number.toInt)
      case plusValue(number) => PlusValue(number.toInt)
    }
    val divider = lines(3) match {case divisibleBy(divider) => divider.toLong}
    val trueId = lines(4) match {case ifTrue(id) => id.toLong}
    val falseId = lines(5) match {case ifFalse(id) => id.toLong}
    Monkey(id, numbers, op, divider, trueId, falseId)

  private def round(monkeys: Array[Monkey], inspections: Array[Long]): Unit =
    monkeys.foreach(m =>
      val ids = m.round()
      inspections(m.id) += ids.length
      ids.indices.foreach(i => monkeys(ids(i).toInt).items += m.items(i))
      m.items.remove(0, ids.length)
    )

  override def exampleAnswerPart1: Long = 10605
  override def solvePart1(lines: List[String]): Long = {
    val monkeys = lines.filter(_.nonEmpty).grouped(6).map(createMonkey).toArray
    Monkey.limiter = monkeys.map(_.divider).product
    val inspections = monkeys.map(_ => 0L)
    (1 to 20).foreach(_ => round(monkeys, inspections))
    val sortedInspections = inspections.sorted.reverse
    sortedInspections.take(2).product
  }

  override def exampleAnswerPart2: Long = 2713310158L
  override def solvePart2(lines: List[String]): Long = {
    val monkeys = lines.filter(_.nonEmpty).grouped(6).map(createMonkey).toArray
    Monkey.safety = 1L
    Monkey.limiter = monkeys.map(_.divider).product
    val inspections = monkeys.map(_ => 0L)
    (1 to 10000).foreach(_ => round(monkeys, inspections))
    val sortedInspections = inspections.sorted.reverse
    sortedInspections.take(2).product
  }

}

object Day11 extends App{
  new Day11().solvePuzzles("/day11.txt")
}
