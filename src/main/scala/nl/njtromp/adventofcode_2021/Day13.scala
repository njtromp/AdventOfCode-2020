package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.util.matching.Regex

class Day13 extends Puzzle {

  val Dot: Regex = "(\\d+),(\\d+)".r
  val FoldY: Regex = "fold along y=(\\d+)".r
  val FoldX: Regex = "fold along x=(\\d+)".r

  private def convertInput(lines: List[String]): (Set[(Int, Int)], List[String]) = {
    val (dots, instructions) = lines.foldLeft((Set.empty[(Int, Int)], List.empty[String]))((ps, l) => l match {
      case Dot(x,y) => (ps._1 ++ Set((x.toInt, y.toInt)), ps._2)
      case _ => (ps._1, l :: ps._2)
    })
    (dots, instructions.filterNot(_.isBlank).reverse)
  }

  private def printPaper(dots: Set[(Int, Int)]): Unit = {
    (dots.map(_._2).min to dots.map(_._2).max).foreach(y => {
      (dots.map(_._1).min to dots.map(_._1).max).foreach(x => if (dots.contains((x, y))) print('#') else print('.'))
      println
    })
  }

  private def foldPaper(dots: Set[(Int, Int)], instruction: String): Set[(Int, Int)] = {
    instruction match {
      case FoldY(y) => dots.map(d => if (d._2 > y.toInt) (d._1, y.toInt - (d._2 - y.toInt)) else d)
      case FoldX(x) => dots.map(d => if (d._1 > x.toInt) (x.toInt - (d._1 - x.toInt), d._2) else d)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val (dots, instructions) = convertInput(lines)
    instructions.take(1).foldLeft(dots)(foldPaper).size
  }

  override def solvePart2(lines: List[String]): Long = {
    val (dots, instructions) = convertInput(lines)
    printPaper(instructions.foldLeft(dots)(foldPaper))
    println("Final Answer: UEFZCUCJ")
    0
  }
}

object Day13 extends App {
  new Day13().solvePuzzles("/2021/day13.txt")
}
