package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day22 extends Puzzle[Long] {

  private case class Generator(seed: Long) {
    private var secret = seed
    private val changes = mutable.Queue.empty[Long]
    private val sells = mutable.Map.empty[List[Long], Long]
    private def prune: Long =
      secret %= 16777216
      secret
    private def mix(n: Long): Long =
      secret ^= n
      secret
    private def next: Long =
      mix(secret * 64)
      prune
      mix(secret / 32)
      prune
      mix(secret * 2048)
      prune
      secret
    @tailrec
    final def simulate(n: Long): Long =
      if n == 0 then
        secret
      else
        next
        simulate(n - 1)
    private def onesDigit: Long = secret % 10
    private def nextChange: Long =
      val old = onesDigit
      next
      onesDigit - old
    @tailrec
    final def registerChanges(n: Long): Map[List[Long], Long] =
      if n == 0 then
        sells.toMap.withDefaultValue(0L)
      else
        if changes.size == 4 then
          val pattern = changes.toList
          if !sells.contains(pattern) then
            sells(pattern) = onesDigit
        changes.enqueue(nextChange)
        if changes.size > 4 then
          changes.dequeue()
        registerChanges(n - 1)
  }

  override def exampleAnswerPart1: Long = 37327623
  override def solvePart1(lines: List[String]): Long =
    (if lines.size == 8 then lines.take(4) else lines).map(line => Generator(line.toLong).simulate(2000)).sum

  override def exampleAnswerPart2: Long = 23
  override def solvePart2(lines: List[String]): Long =
    val possibleSells = (if lines.size == 8 then lines.drop(4) else lines).map(line => Generator(line.toLong).registerChanges(2000))
    val patterns = possibleSells.flatMap(_.keySet).toSet
    val sellsByPattern = patterns.map(pattern => (pattern, possibleSells.map(_(pattern)).sum))
    val bestPattern = sellsByPattern.maxBy(_._2)
    bestPattern._2

}

object Day22 extends App {
  new Day22().solvePuzzles()
}
