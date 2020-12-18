package nl.njtromp.adventofcode_2020

import scala.Array.ofDim

class Day17 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    val gridSize = lines.size
    val grid = createGrid(gridSize)
    lines.zipWithIndex.foreach(
      {case (line, y) => line.zipWithIndex.foreach(
        {case (cell, x) => grid(gridSize / 2)(y)(x) = cell})})

    showGrid(grid)
    solve(grid, 6).map(_.map(_.count(_ == '#')).sum).sum
  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

  private def solve(grid: Array[Array[Array[Char]]], cyles: Int): Array[Array[Array[Char]]] = {
    println("="*80)
    val size = grid.length
    val newGrid = createGrid(size)
    for (z <- grid.indices)
      for (y <- grid.indices)
        for (x <- grid.indices)
          grid(z)(y)(x) match {
            case '#' => newGrid(z)(y)(x) = if ((2 to 3).contains(countNeighbors(grid, z, y, x, 1, '#'))) '#' else '.'
            case '.' => newGrid(z)(y)(x) = if (countNeighbors(grid, z, y, x, 1, '#') == 3) '#' else '.'
          }
    showGrid(newGrid)
    if (cyles == 1) newGrid else solve(newGrid, cyles - 1)
  }

  private def countNeighbors(grid: Array[Array[Array[Char]]], z: Int, y: Int, x: Int, length: Int, c: Char): Int= {
    def cap(c: Int): Int = (c + grid.length) % grid.length
//    var matching: Int = 0
//    for (dz <- -length to length)
//      for (dy <- -length to length)
//        for (dx <- -length to length)
//          if (Math.abs(dz) + Math.abs(dy) + Math.abs(dx) <= 2) {
//            if (dz != 0 && dy != 0 && dx != 0) {
//              if (grid(cap(z + dz))(cap(y + dy))(cap(x + dx)) == c) {
//                matching += 1
//              }
//            }
//          }
    List(
      grid(cap(z-1))(y)(x), grid(cap(z+1))(y)(x),
      grid(z)(cap(y-1))(x), grid(z)(cap(y+1))(x),
      grid(z)(y)(cap(x-1)), grid(z)(y)(cap(x+1)),
    ).count(_ == c)
  }

  private def createGrid(size: Int): Array[Array[Array[Char]]] = {
    val grid = ofDim[Char](size, size, size)
    for (z <- 0 until size)
      for (y <- 0 until size)
        for (x <- 0 until size)
          grid(z)(y)(x) = '.'
    grid
  }

  private def showGrid(grid: Array[Array[Array[Char]]]): Unit = {
    for (z <- grid.indices) {
      println(s"Z=${z - grid.length / 2}")
      for (y <- grid.indices)
        println(grid(z)(y).mkString)
      println
    }
  }
}

object Day17 extends App {
  new Day17().solvePuzzles("/input-puzzle17.txt")
}


