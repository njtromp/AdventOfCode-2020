package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.{Puzzle2, SimpleMap, SimpleMapTypes}

import scala.annotation.tailrec

class Day18 extends Puzzle2 with SimpleMapTypes{
  private val OPEN_GROUND = '.'
  private val TREE = '|'
  private val LUMBERYARD = '#'

  @tailrec
  private def evolve(forrest: SimpleMap[Char], minute: Long, minutes: Long): SimpleMap[Char] = {
    val trees = forrest.elems.map(_.count(_ == TREE)).sum
    val lumberyards = forrest.elems.map(_.count(_ == LUMBERYARD)).sum
    if (minute == minutes || (trees == 0 && lumberyards == 0))
      forrest
    else {
      val newForrest = new SimpleMap[Char](Array.ofDim[Char](forrest.height, forrest.width))
      (0 until forrest.height).foreach(y =>
        (0 until forrest.width).foreach(x => {
          val pos = (y, x)
          forrest(pos) match {
            case OPEN_GROUND => newForrest(pos) =
              if (forrest.neighbors(pos, all).count(_ == TREE) >= 3) TREE else OPEN_GROUND
            case TREE => newForrest(pos) =
              if (forrest.neighbors(pos, all).count(_ == LUMBERYARD) >= 3) LUMBERYARD else TREE
            case LUMBERYARD => newForrest(pos) =
              if (forrest.neighbors(pos, all).count(_ == LUMBERYARD) >= 1 && forrest.neighbors(pos, all).count(_ == TREE) >= 1) LUMBERYARD else OPEN_GROUND
          }
        })
      )
      evolve(newForrest, minute + 1, minutes)
    }
  }

  override def exampleAnswerPart1: Long = 1147
  override def solvePart1(lines: List[String]): Long = {
    val forrest = evolve(SimpleMap[Char](lines, _.toArray), 0, 10L)
    forrest.elems.map(_.count(_ == TREE)).sum * forrest.elems.map(_.count(_ == LUMBERYARD)).sum
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    // Pattern starts repeating after about 500 minutes
    val minutes = 500 + (1000000000L - 500L) % 28L
    val forrest = evolve(SimpleMap[Char](lines, _.toArray), 0, minutes)
    forrest.elems.map(_.count(_ == TREE)).sum * forrest.elems.map(_.count(_ == LUMBERYARD)).sum
  }

  private def printForrest(minute: Long, forrest: SimpleMap[Char]): Unit = {
    println(s"After $minute minute${if (minute > 1) "s" else ""}")
    (0 until forrest.height).foreach(y => println(forrest.row(y).mkString))
    println
  }

}

object Day18 extends App {
  new Day18().solvePuzzles("/2018/day18.txt")
}
