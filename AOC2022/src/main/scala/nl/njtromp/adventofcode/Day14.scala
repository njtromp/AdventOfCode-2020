package nl.njtromp.adventofcode

class Day14 extends Puzzle[Long] {
  private val ROCK = '#'
  private val AIR = '.'
  private val SAND = 'o'
  private val START = '+'

  private def createCaveSystem(scan: Array[Array[Char]], coordinates: List[List[(Int, Int)]]): Unit =
    coordinates.foreach(cs => cs.zip(cs.tail).foreach((s,f) =>
      val deltaX = Math.signum(f._1 - s._1).toInt
      if (deltaX == 0)
        (s._2 to f._2 by Math.signum(f._2 - s._2).toInt).foreach(y => scan(y)(s._1) = ROCK)
      else
        (s._1 to f._1 by deltaX).foreach(x => scan(s._2)(x) = ROCK)
    ))

  private def pourSand(scan: Array[Array[Char]], startX: Int, foundSolution: (Int, Int) => Boolean): Unit =
    var y = 0
    var x = startX
    while (!foundSolution(y, x))
      while (y + 1 < scan.length && scan(y + 1)(x) == AIR)
        y += 1
      if (y + 1< scan.length && scan(y + 1)(x - 1) == AIR)
        y += 1
        x -= 1
      else if (y + 1< scan.length && scan(y + 1)(x + 1) == AIR)
        y += 1
        x += 1
      else if (y + 1 < scan.length)
        scan(y)(x) = SAND
        y = 0
        x = startX
      else
        y = scan.length

  override def exampleAnswerPart1: Long = 24
  override def solvePart1(lines: List[String]): Long = {
    val coordinates = lines.map(_.split(" -> ").toList.map(_.trim.split(",").map(_.toInt)).map(ps => (ps.head, ps.last)))
    val minX = coordinates.map(_.map(_._1).min).min - 2
    val maxX = coordinates.map(_.map(_._1).max).max + 2
    val maxY = coordinates.map(_.map(_._2).max).max + 2
    val deltaX = maxX - minX
    val scan = Array.fill[Char](maxY, deltaX)(AIR)
    createCaveSystem(scan, coordinates.map(_.map(c => (c._1 - minX, c._2))))
    val startX = 500 - minX
    scan(0)(startX) = START
    pourSand(scan, startX, (y, _) => y == scan.length)
    scan.map(_.count(_ == SAND)).sum
  }

  override def exampleAnswerPart2: Long = 93
  override def solvePart2(lines: List[String]): Long = {
    val coordinates = lines.map(_.split(" -> ").toList.map(_.trim.split(",").map(_.toInt)).map(ps => (ps.head, ps.last)))
    val maxY = coordinates.map(_.map(_._2).max).max + 3
    val deltaX = 1000
    val scan = Array.fill[Char](maxY + 1, deltaX + 1)(AIR)
    createCaveSystem(scan, List(List((0, maxY - 1), (1000, maxY - 1))) ++ coordinates)
    pourSand(scan, 500, (y, x) => scan(y)(x) == SAND)
    scan.map(_.count(_ == SAND)).sum
  }

  private def printScan(scan: Array[Array[Char]]): Unit =
    println
    scan.foreach(y =>
      y.foreach(print)
      println
    )

}

object Day14 extends App{
  new Day14().solvePuzzles("/day14.txt")
}
