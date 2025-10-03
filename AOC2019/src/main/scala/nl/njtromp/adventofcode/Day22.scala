package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day22 extends Puzzle[Long] {

  private def newStack(cards: List[Int]): List[Int] = cards.reverse

  private def cut(n: Int, cards: List[Int]): List[Int] =
    if n >= 0 then
      cards.drop(n) ++ cards.take(n)
    else
      cards.takeRight(-n) ++ cards.dropRight(-n)

  private def deal(n: Int, cards: List[Int]): List[Int] =
    val newDeck = Array.fill(cards.size)(0)
    @tailrec
    def deal(i: Int, cards: List[Int]): Unit =
      if cards.nonEmpty then
        newDeck(i) = cards.head
        deal((i + n) % newDeck.length, cards.tail)
    deal(0, cards)
    newDeck.toList

  override def exampleAnswerPart1: Long = 2019
  override def solvePart1(lines: List[String]): Long =
    val deck = (0 until 10007).toList
    val result = lines.foldLeft(deck)((cards, ins) => ins match
      case "deal into new stack" =>
        newStack(cards)
      case s"cut $n" =>
        cut(n.toInt, cards)
      case s"deal with increment $n" =>
        deal(n.toInt, cards)
    )
    println(result.mkString(", "))
    result.zipWithIndex.find(_._1 == 2019).get._2

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
//    val deck = LongRange(0, 101741582076661L - 1L).toList
//    val result = lines.foldLeft(deck)((cards, ins) => ins match
//      case "deal into new stack" =>
//        newStack(cards)
//      case s"cut $n" =>
//        cut(n.toInt, cards)
//      case s"deal with increment $n" =>
//        deal(n.toInt, cards)
//    )
//    println(result.mkString(", "))
//    result.zipWithIndex.find(_._1 == 2019).get._2
    -1

}

object Day22 extends App {
  new Day22().solvePuzzles()
}
