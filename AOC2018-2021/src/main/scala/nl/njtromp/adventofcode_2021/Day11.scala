package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day11 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val flashMap: Array[Array[Boolean]] = new Array(lines.length)
    flashMap.indices.foreach(y => flashMap(y) = new Array(lines(y).length))
    @tailrec
    def evolve(step: Int, flashes: Long, octos: Array[Array[Int]]): Long = {
      def isOnMap(x: Int, y: Int): Boolean = {
        y >= 0 && y < octos.length && x >= 0 && x < octos(y).length
      }
      def flash(x: Int, y: Int): Unit = {
        List((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)).
          foreach(d => if (isOnMap(x + d._1, y + d._2)) octos(y + d._2)(x + d._1) = octos(y + d._2)(x + d._1) + 1)
      }
      if (step == 0)
        flashes
      else {
        octos.indices.foreach(y => octos(y).indices.foreach(x => {
          octos(y)(x) = octos(y)(x) + 1
          flashMap(y)(x) = false
        }))
        var hasFlashed = false
        while {
          hasFlashed = false
          octos.indices.foreach(y => octos(y).indices.foreach(x => {
            if (octos(y)(x) > 9 && !flashMap(y)(x)) {
              hasFlashed = true
              flashMap(y)(x) = true
              flash(x, y)
            }
          }))
          hasFlashed
        } do()
//        do {
//          hasFlashed = false
//          octos.indices.foreach(y => octos(y).indices.foreach(x => {
//            if (octos(y)(x) > 9 && !flashMap(y)(x)) {
//              hasFlashed = true
//              flashMap(y)(x) = true
//              flash(x, y)
//            }
//          }))
//        } while (hasFlashed)
        octos.indices.foreach(y => octos(y).indices.foreach(x => if (octos(y)(x) > 9) octos(y)(x) = 0))
        evolve(step - 1, flashes + flashMap.map(_.count(_ == true)).sum, octos)
      }
    }
    evolve(100, 0L, lines.map(_.toArray.map(_.asDigit)).toArray)
  }

  override def solvePart2(lines: List[String]): Long = {
    val flashMap: Array[Array[Boolean]] = new Array(lines.length)
    flashMap.indices.foreach(y => flashMap(y) = new Array(lines(y).length))
    val totalOctos = lines.length * lines.head.length
    @tailrec
    def evolve(step: Long, octos: Array[Array[Int]]): Long = {
      def isOnMap(x: Int, y: Int): Boolean = {
        y >= 0 && y < octos.length && x >= 0 && x < octos(y).length
      }
      def flash(x: Int, y: Int): Unit = {
        List((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)).
          foreach(d => if (isOnMap(x + d._1, y + d._2)) octos(y + d._2)(x + d._1) = octos(y + d._2)(x + d._1) + 1)
      }
      octos.indices.foreach(y => octos(y).indices.foreach(x => {
        octos(y)(x) = octos(y)(x) + 1
        flashMap(y)(x) = false
      }))
      var hasFlashed = false
      while {
        hasFlashed = false
        octos.indices.foreach(y => octos(y).indices.foreach(x => {
          if (octos(y)(x) > 9 && !flashMap(y)(x)) {
            hasFlashed = true
            flashMap(y)(x) = true
            flash(x, y)
          }
        }))
        hasFlashed
      } do ()
//      do {
//        hasFlashed = false
//        octos.indices.foreach(y => octos(y).indices.foreach(x => {
//          if (octos(y)(x) > 9 && !flashMap(y)(x)) {
//            hasFlashed = true
//            flashMap(y)(x) = true
//            flash(x, y)
//          }
//        }))
//      } while (hasFlashed)
      octos.indices.foreach(y => octos(y).indices.foreach(x => if (octos(y)(x) > 9) octos(y)(x) = 0))
      if (flashMap.map(_.count(_ == true)).sum == totalOctos)
        step
      else
        evolve(step + 1, octos)
    }
    evolve(1L, lines.map(_.toArray.map(_.asDigit)).toArray)
  }
}

object Day11 extends App {
  new Day11().solvePuzzles("/2021/day11.txt")
}
