package nl.njtromp.adventofcode_2020

import scala.Array.ofDim
import scala.annotation.tailrec

class Day17 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    val gridSize = lines.size + 4
    val grid = createGrid3D(gridSize)
    lines.zipWithIndex.foreach(
      {case (line, y) => line.zipWithIndex.foreach(
        {case (cell, x) => grid(gridSize / 2)(y + 2)(x + 2) = cell})}
    )
    solve3D(grid, 6).map(_.map(_.count(_.equals('#'))).sum).sum
  }

  def solvePart2(lines: List[String]): Long = {
    val gridSize = lines.size + 4
    val grid = createGrid4D(gridSize)
    lines.zipWithIndex.foreach(
      {case (line, y) => line.zipWithIndex.foreach(
        {case (cell, x) => grid(gridSize / 2)(gridSize / 2)(y + 2)(x + 2) = cell})}
    )
    solve4D(grid, 6).map(_.map(_.map(_.count(_.equals('#'))).sum).sum).sum
  }

  @tailrec
  private def solve3D(grid: Array[Array[Array[Char]]], cyles: Int): Array[Array[Array[Char]]] = {
    val newGrid = createGrid3D(grid.length + 2)
    for (z <- grid.indices)
      for (y <- grid.indices)
        for (x <- grid.indices)
          grid(z)(y)(x) match {
            case '.' => newGrid(z + 1)(y + 1)(x + 1) = if (countNeighbors3D(grid, z, y, x) == 3) '#' else '.'
            case '#' => newGrid(z + 1)(y + 1)(x + 1) = if ((2 to 3).contains(countNeighbors3D(grid, z, y, x))) '#' else '.'
          }
    if (cyles == 1) newGrid else solve3D(newGrid, cyles - 1)
  }

  private def countNeighbors3D(grid: Array[Array[Array[Char]]], z: Int, y: Int, x: Int): Int= {
    var matching: Int = 0
    for (dz <- -1 to 1)
      for (dy <- -1 to 1)
        for (dx <- -1 to 1)
          if (grid.indices.contains(z + dz) && grid.indices.contains(y + dy) && grid.indices.contains(x + dx)) {
            if (!(dz == 0 && dy == 0 && dx == 0)) {
              if (grid(z + dz)(y + dy)(x + dx).equals('#')) {
                matching += 1
              }
            }
          }
    matching
  }

  private def createGrid3D(size: Int): Array[Array[Array[Char]]] = {
    val grid = ofDim[Char](size, size, size)
    for (z <- 0 until size)
      for (y <- 0 until size)
        for (x <- 0 until size)
          grid(z)(y)(x) = '.'
    grid
  }

  @tailrec
  private def solve4D(grid: Array[Array[Array[Array[Char]]]], cyles: Int): Array[Array[Array[Array[Char]]]] = {
    val newGrid = createGrid4D(grid.length + 2)
    for (w <- grid.indices)
      for (z <- grid.indices)
        for (y <- grid.indices)
          for (x <- grid.indices)
            grid(w)(z)(y)(x) match {
              case '.' => newGrid(w + 1)(z + 1)(y + 1)(x + 1) = if (countNeighbors4D(grid, w, z, y, x) == 3) '#' else '.'
              case '#' => newGrid(w + 1)(z + 1)(y + 1)(x + 1) = if ((2 to 3).contains(countNeighbors4D(grid, w, z, y, x))) '#' else '.'
            }
    if (cyles == 1) newGrid else solve4D(newGrid, cyles - 1)
  }

  private def countNeighbors4D(grid: Array[Array[Array[Array[Char]]]], w: Int, z: Int, y: Int, x: Int): Int= {
    var matching: Int = 0
    for (dw <- -1 to 1)
      for (dz <- -1 to 1)
        for (dy <- -1 to 1)
          for (dx <- -1 to 1)
            if (grid.indices.contains(w + dw) && grid.indices.contains(z + dz) && grid.indices.contains(y + dy) && grid.indices.contains(x + dx)) {
              if (!(dw == 0 && dz == 0 && dy == 0 && dx == 0)) {
                if (grid(w + dw)(z + dz)(y + dy)(x + dx).equals('#')) {
                  matching += 1
                }
              }
            }
    matching
  }

  private def createGrid4D(size: Int): Array[Array[Array[Array[Char]]]] = {
    val grid = ofDim[Char](size, size, size, size)
    for (w <- 0 until size)
      for (z <- 0 until size)
        for (y <- 0 until size)
          for (x <- 0 until size)
            grid(w)(z)(y)(x) = '.'
    grid
  }
}

object Day17 extends App {
  new Day17().solvePuzzles("/input-puzzle17.txt")
}
