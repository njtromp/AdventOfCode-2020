package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.{Puzzle2, SimpleMap}

class Day17 extends Puzzle2 {
  type Pos = (Int, Int)
  private val CLAY = "(x|y)=(\\d+), .=(\\d+)\\.\\.(\\d+)".r
  private var minX = 0
  private var maxX = 0
  private var maxY = 0

  private def createScan(lines: List[String]): SimpleMap[Char] = {
    val clay = lines.flatMap({
      case CLAY(xy, start, low, high) => if (xy == "x")
        (low.toInt to high.toInt).map((_, start.toInt)).toList
      else
        (low.toInt to high.toInt).map((start.toInt, _)).toList
    }).toSet
    minX = clay.minBy(_._2)._2 - 1
    maxX = clay.maxBy(_._2)._2 + 2
    maxY = clay.maxBy(_._1)._1
    val scan = new SimpleMap[Char](Array.fill[Char](maxY + 1, maxX - minX)('.'))
    scan((0, 500 - minX)) = '+'
    clay.foreach(c => scan((c._1, c._2 - minX)) = '#')
    scan
  }

  private def letItFlow(source: Pos, scan: SimpleMap[Char], leftLimit: Int, rightLimit: Int): Unit = {
    printScan(scan)
    val sourceY = source._1
    val sourceX = source._2
    val column = scan.column(sourceX)
    val clayLayer = sourceY + column.drop(sourceY + 1).takeWhile(_ == '.').length
    if (clayLayer == maxY)
      (sourceY + 1 to maxY).foreach(y => scan((y, sourceX)) = '|')
    else {
      val row = scan.row(clayLayer)
      val maxValidX = scan.width - 1
      val leftSide = sourceX - row.take(sourceX).reverse.takeWhile(_ == '.').length
      val rightSide = sourceX + row.drop(sourceX).takeWhile(_ == '.').length - 1
      (leftSide == 0, rightSide == maxValidX) match {
        case (false, true) =>
          (sourceY + 1 until clayLayer).foreach(y => scan((y, sourceX)) = '|')
          (leftSide to rightSide).foreach(x => scan((clayLayer, x)) = '|')
          if (clayLayer < scan.height - 1 && scan.row(clayLayer + 1)(rightSide) == '.')
            letItFlow((clayLayer - 1, rightSide), scan, 0, scan.width - 1)
        case (true, false) =>
          (sourceY + 1 until clayLayer).foreach(y => scan((y, sourceX)) = '|')
          (leftLimit to rightSide).foreach(x => scan((clayLayer, x)) = '|')
          if (clayLayer < scan.height - 1 && scan.row(clayLayer + 1)(leftSide) == '.') {
            letItFlow((clayLayer - 1, leftSide), scan, 0, scan.width - 1)
          }
        case (_, _) =>
          if (leftLimit <= leftSide && rightSide <= rightLimit) {
            (leftSide to rightSide).foreach(x => scan((clayLayer, x)) = '~')
            letItFlow(source, scan, leftSide, rightSide)
          } else {
            val overflowLeft = sourceX - 1 - scan.row(clayLayer + 1).take(sourceX).reverse.takeWhile(_ != '.').length
            val overflowRight = sourceX + scan.row(clayLayer + 1).drop(sourceX).takeWhile(_ != '.').length
            (sourceY + 1 until clayLayer).foreach(y => scan((y, sourceX)) = '|')
            (Math.max(overflowLeft, leftSide) to Math.min(overflowRight, rightSide)).foreach(x => scan((clayLayer, x)) = '|')
            if (leftSide < overflowLeft)
              letItFlow((clayLayer, overflowLeft), scan, 0, scan.width - 1)
            if (overflowLeft < rightSide)
              letItFlow((clayLayer, overflowRight), scan, 0, scan.width - 1)
          }
      }
    }
  }

  override def exampleAnswerPart1: Long = 57
  override def solvePart1(lines: List[String]): Long = {
    val scan = createScan(lines)
    printScan(scan)
    letItFlow((0, 500 - minX), scan, 0, scan.width - 1)
    printScan(scan)
    scan.elems.map(_.count("~|".contains(_))).sum
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    -1
  }

  private def printScan(scan: SimpleMap[Char]): Unit = {
    print(' ')
    (0 to scan.width).foreach(x => print(x % 10))
    println
    (0 until scan.height).foreach(y => {
      print(y % 10)
      println(scan.row(y).mkString)
    })
    println
  }

}

object Day17 extends App {
  new Day17().solvePuzzles("/2018/day17.txt")
}
