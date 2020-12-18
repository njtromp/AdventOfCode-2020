package nl.njtromp.adventofcode_2020

import scala.Array.ofDim
import scala.annotation.tailrec

class Day17 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    val gridSize = lines.size + 4
    val grid = createGrid(gridSize)
    lines.zipWithIndex.foreach(
      {case (line, y) => line.zipWithIndex.foreach(
        {case (cell, x) => grid(gridSize / 2)(y + 2)(x + 2) = cell})}
    )
    solve(grid, 6).map(_.map(_.count(_.equals('#'))).sum).sum
  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

  @tailrec
  private def solve(grid: Array[Array[Array[Char]]], cyles: Int): Array[Array[Array[Char]]] = {
    val newGrid = createGrid(grid.length + 2)
    for (z <- grid.indices) {
      for (y <- grid.indices) {
        for (x <- grid.indices) {
          grid(z)(y)(x) match {
            case '.' => newGrid(z + 1)(y + 1)(x + 1) = if (countNeighbors(grid, z, y, x) == 3) '#' else '.'
            case '#' => newGrid(z + 1)(y + 1)(x + 1) = if ((2 to 3).contains(countNeighbors(grid, z, y, x))) '#' else '.'
          }
        }
      }
    }
    if (cyles == 1) newGrid else solve(newGrid, cyles - 1)
  }

  private def countNeighbors(grid: Array[Array[Array[Char]]], z: Int, y: Int, x: Int): Int= {
    var matching: Int = 0
    for (dz <- -1 to 1) {
      for (dy <- -1 to 1) {
        for (dx <- -1 to 1) {
          if (grid.indices.contains(z + dz) && grid.indices.contains(y + dy) && grid.indices.contains(x + dx)) {
            if (!(dz == 0 && dy == 0 && dx == 0)) {
              if (grid(z + dz)(y + dy)(x + dx).equals('#')) {
                matching += 1
              }
            }
          }
        }
      }
    }
    matching
  }

  private def createGrid(size: Int): Array[Array[Array[Char]]] = {
    val grid = ofDim[Char](size, size, size)
    for (z <- 0 until size) {
      for (y <- 0 until size) {
        for (x <- 0 until size) {
          grid(z)(y)(x) = '.'
        }
      }
    }
    grid
  }

  private def showGrid(grid: Array[Array[Array[Char]]]): Unit = {
    for (z <- grid.indices) {
      println(s"Z=${z - grid.length / 2}")
      for (y <- grid.indices) {
        println(grid(z)(y).mkString)
      }
      println
    }
  }
}

object Day17 extends App {
  new Day17().solvePuzzles("/input-puzzle17.txt")
}


