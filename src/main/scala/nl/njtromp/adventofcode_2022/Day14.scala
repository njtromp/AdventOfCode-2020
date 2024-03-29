package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.{Puzzle2, SimpleMapTypes}

import scala.annotation.tailrec

class Day14 extends Puzzle2 with SimpleMapTypes {
  private val POSITION = "(\\d+),(\\d+)".r
  private val START = '+'
  private val AIR = '.'
  private val ROCK = '#'
  private val SAND = 'o'

  private def createPos(pos: String): Pos = pos match {
    case POSITION(x, y) => (y.toInt, x.toInt)
  }

  private def createLineSeqments(lines: List[String]):List[List[Pos]] = {
    lines.map(_.split("->").toList.map(_.trim))
      .map(_.map(createPos))
  }

  private def createCaveMap(lineSements: List[List[(Pos, Pos)]], height: Int, width: Int, sourceX: Int): Array[Array[Char]] = {
    val caveMap = Array.fill(height, width)(AIR)
    def createLine(segments: List[(Pos, Pos)]): Unit = {
      segments.foreach(s => {
        val minY = Math.min(s._1._1, s._2._1)
        val maxY = Math.max(s._1._1, s._2._1)
        val minX = Math.min(s._1._2, s._2._2)
        val maxX = Math.max(s._1._2, s._2._2)
        if (minX == maxX)
          (minY to maxY).foreach(y => caveMap(y)(minX) = ROCK)
        else
          (minX to maxX).foreach(x => caveMap(minY)(x) = ROCK)
      })
    }

    caveMap(0)(sourceX) = START
    lineSements.foreach(createLine)
    caveMap
  }

  private def createCaveMap2(lineSements: List[List[(Pos, Pos)]], height: Int, width: Int, sourceX: Int): Array[Array[Char]] = {
    val caveMap = createCaveMap(lineSements, height, width, sourceX)
    (0 until width).foreach(x => caveMap(height - 1)(x) = ROCK)
    caveMap
  }

  private def animateSand(caveMap: Array[Array[Char]], sourceX: Int): Int = {
    @tailrec
    def dropGrain(pos: Pos): Pos = {
      val newPos = if (pos._1 == caveMap.length - 1)
        pos
      else if (caveMap(pos._1 + 1)(pos._2) == AIR)
        (pos._1 + 1, pos._2)
      else if (caveMap(pos._1 + 1)(pos._2 - 1) == AIR)
        (pos._1 + 1, pos._2 - 1)
      else if (caveMap(pos._1 + 1)(pos._2 + 1) == AIR)
        (pos._1 + 1, pos._2 + 1)
      else
        pos
      if (newPos == pos)
        pos
      else
        dropGrain(newPos)
    }
    @tailrec
    def dropGrains(grainCount:Int, pos : Pos): Int = {
      val newPos = dropGrain(pos)
      if (newPos._1 == caveMap.length - 1 || newPos == (0, sourceX))
        grainCount
      else {
        caveMap(newPos._1)(newPos._2) = SAND
        dropGrains(grainCount + 1, (0, sourceX))
      }
    }
    dropGrains(1, (0, sourceX))
  }

  override def exampleAnswerPart1: Long = 24
  override def solvePart1(lines: List[String]): Long = {
    val points = createLineSeqments(lines)
    val minX = points.map(_.minBy(_._2)).minBy(_._2)._2
    val maxX = points.map(_.maxBy(_._2)).maxBy(_._2)._2
    val maxY = points.map(_.maxBy(_._1)).maxBy(_._1)._1
    val offsetX = minX - 4
    val sourceX = 500 - offsetX
    val lineSeqments = points.map(_.map(p => (p._1, p._2 - offsetX))).map(l => l.zip(l.tail))
    val caveMap = createCaveMap(lineSeqments, maxY + 3, maxX - minX + 6, sourceX)
    animateSand(caveMap, sourceX) - 1
  }

  override def exampleAnswerPart2: Long = 93
  override def solvePart2(lines: List[String]): Long = {
    val points = createLineSeqments(lines)
    val maxX = points.map(_.maxBy(_._2)).maxBy(_._2)._2
    val maxY = points.map(_.maxBy(_._1)).maxBy(_._1)._1
    val sourceX = 500
    val lineSeqments = points.map(_.map(p => (p._1, p._2))).map(l => l.zip(l.tail))
    val caveMap = createCaveMap2(lineSeqments, maxY + 3, maxX * 2, sourceX)
    animateSand(caveMap, sourceX)
  }

  def printCave(caveMap: Array[Array[Char]]): Unit = {
    caveMap.foreach(l => println(l.mkString))
  }

}

object Day14 extends App{
  new Day14().solvePuzzles("/2022/day14.txt")
}
