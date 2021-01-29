package nl.njtromp.adventofcode_2020

import scala.collection.mutable

class Day22 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val deck1: mutable.Queue[Long] = mutable.Queue.empty
    val deck2: mutable.Queue[Long] = mutable.Queue.empty
    createDecks(lines, deck1, deck2)

    while (deck1.nonEmpty && deck2.nonEmpty) {
      val card1 = deck1.dequeue()
      val card2 = deck2.dequeue()
      if (card1 > card2) {
        deck1.enqueue(card1)
        deck1.enqueue(card2)
      } else {
        deck2.enqueue(card2)
        deck2.enqueue(card1)
      }
    }
    (if (deck1.nonEmpty) deck1 else deck2).reverse.zipWithIndex.map(c => c._1 * (c._2 + 1)).sum
  }

  override def solvePart2(lines: List[String]): Long = {
    val deck1: mutable.Queue[Long] = mutable.Queue.empty
    val deck2: mutable.Queue[Long] = mutable.Queue.empty
    createDecks(lines, deck1, deck2)

    recursiveCombat(deck1, deck2)
    (if (deck1.nonEmpty) deck1 else deck2).reverse.zipWithIndex.map(c => c._1 * (c._2 + 1)).sum
  }

  def createDecks(lines: List[String], deck1: mutable.Queue[Long], deck2: mutable.Queue[Long]): Unit = {
    var deck = deck1
    for (line <- lines) {
      if (line.isEmpty) {
        deck = deck2
      } else if (line.startsWith("Player")) {
      } else {
        deck.enqueue(line.toLong)
      }
    }
  }

  // Returns true if player 1 wins
  private def recursiveCombat(deck1: mutable.Queue[Long], deck2: mutable.Queue[Long]): Boolean = {
    val configs1: mutable.HashSet[List[Long]] = mutable.HashSet.empty
    val configs2: mutable.HashSet[List[Long]] = mutable.HashSet.empty

    while (deck1.nonEmpty && deck2.nonEmpty) {
      if (configs1.contains(deck1.toList) || configs2.contains(deck2.toList)) {
        return true
      } else {
        configs1 += deck1.toList
        configs2 += deck2.toList
        val card1 = deck1.dequeue()
        val card2 = deck2.dequeue()
        if (if (card1 <= deck1.size && card2 <= deck2.size) recursiveCombat(deck1.clone().take(card1.toInt), deck2.clone().take(card2.toInt)) else card1 > card2) {
          deck1.enqueue(card1)
          deck1.enqueue(card2)
        } else {
          deck2.enqueue(card2)
          deck2.enqueue(card1)
        }
      }
    }
    deck1.nonEmpty
  }
}

object Day22 extends App {
  new Day22().solvePuzzles("/2020/input-puzzle22.txt")
}
