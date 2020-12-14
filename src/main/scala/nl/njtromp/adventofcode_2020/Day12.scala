package nl.njtromp.adventofcode_2020

import scala.io.Source

class Day12 {
  private val EW = "([EW])(\\d+)".r
  private val NS = "([NS])(\\d+)".r
  private val LR = "([LR])(\\d+)".r
  private val F = "(F)(\\d+)".r

  def solvePart1(lines: List[String]): Int = {
    var heading: Int = 0
    var pos: (Int, Int) = (0, 0)
    for (line <- lines) {
      line match {
        case EW(d, dx) => pos = (pos._1 + (if (d == "E") 1 else -1) * dx.toInt, pos._2)
        case NS(d, dy) => pos = (pos._1, pos._2 + (if (d == "N") 1 else -1) * dy.toInt)
        case LR(r, dh) => heading = (360 + heading + (if (r == "L") 1 else -1) * dh.toInt) % 360
        case F(_, f) => heading match {
          case 0 => pos = (pos._1 + f.toInt, pos._2)
          case 90 => pos =(pos._1, pos._2 + f.toInt)
          case 180 => pos = (pos._1 - f.toInt, pos._2)
          case 270 => pos =(pos._1, pos._2 - f.toInt)
        }
      }
    }
    Math.abs(pos._1) + Math.abs(pos._2)
  }

  private val COUNTER_CLOCKWISE = "(L90)|(R270)".r
  private val CLOCKWISE = "(R90)|(L270)".r
  private val REVERSE = "(L180)|(R180)".r

  def solvePart2(lines: List[String]): Int = {
    var pos = (0, 0)
    var waypoint = (10, 1)
    for (line <- lines) {
      line match {
        case EW(d, dx) => waypoint = (waypoint._1 + (if (d == "E") 1 else -1) * dx.toInt, waypoint._2)
        case NS(d, dy) => waypoint = (waypoint._1, waypoint._2 + (if (d == "N") 1 else -1) * dy.toInt)
        case COUNTER_CLOCKWISE(_,_) => waypoint = (-waypoint._2, waypoint._1)
        case CLOCKWISE(_, _) => waypoint = (waypoint._2, -waypoint._1)
        case REVERSE(_, _) => waypoint = (-waypoint._1, -waypoint._2)
        case F(_, f) => pos = (pos._1 + waypoint._1 * f.toInt, pos._2 + waypoint._2 * f.toInt)
      }
    }
    Math.abs(pos._1) + Math.abs(pos._2)
  }

  def solvePuzzles(): Unit = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream("/input-puzzle12.txt")).getLines.toList
    println(s"Answer part 1: ${solvePart1(lines)}")
    println(s"Answer part 2: ${solvePart2(lines)}")
  }
}

object Day12 extends App {
  new Day12().solvePuzzles()
}
