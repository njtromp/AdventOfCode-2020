package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Day14 extends Puzzle2 {

  @tailrec
  private def evolveRecipes(scores: ArrayBuffer[Int], limit: Int, first: Int, second: Int): Unit = {
    if (scores.length < limit + 10) {
      val firstScore = scores(first)
      val secondScore = scores(second)
      val newRecipe = firstScore + secondScore
      if (newRecipe > 9) {
        scores.addOne(newRecipe / 10)
      }
      scores.addOne(newRecipe % 10)
      evolveRecipes(scores, limit, (1 + first + firstScore) % scores.length, (1 + second + secondScore) % scores.length)
    }
  }

  @tailrec
  private def evolveRecipes(scores: ArrayBuffer[Int], marker: Array[Int], first: Int, second: Int): Long = {
    val markerLength = marker.length
    def indexOfMarker(): Int = {
      val start = if (scores.length >= markerLength && scores(scores.length - markerLength) == marker(0))
        scores.length - markerLength
      else if (scores.length > markerLength && scores(scores.length - markerLength - 1) == marker(0))
        scores.length - markerLength - 1
      else
        -1
      if (start >= 0 && (0 until markerLength).forall(n => scores(start + n) == marker(n)))
        start
      else
        -1
    }
    val recipesToTheLeft = indexOfMarker()
    if (recipesToTheLeft > 0)
      recipesToTheLeft
    else {
      val firstScore = scores(first)
      val secondScore = scores(second)
      val newRecipe = firstScore + secondScore
      if (newRecipe > 9) {
        scores.addOne(newRecipe / 10)
      }
      scores.addOne(newRecipe % 10)
      evolveRecipes(scores, marker, (1 + first + firstScore) % scores.length, (1 + second + secondScore) % scores.length)
    }
  }

  override def exampleAnswerPart1: Long = 5158916779L
  override def solvePart1(lines: List[String]): Long = {
    val limit = lines.head.toInt
    val scores = ArrayBuffer(3, 7)
    evolveRecipes(scores, limit, 0, 1)
    scores.slice(limit, limit + 10).foldLeft(0L)((a, n) => a * 10 + n)
  }

  override def exampleAnswerPart2: Long = 9
  override def solvePart2(lines: List[String]): Long = {
    val marker = if (lines.head == "9") "51589" else lines.head
    evolveRecipes(ArrayBuffer(3, 7), marker.toArray.map(_.asDigit), 0, 1)
  }

}

object Day14 extends App {
  new Day14().solvePuzzles("/2018/day14.txt")
}
