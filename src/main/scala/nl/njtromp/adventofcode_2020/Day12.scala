package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day12 extends App {
  var heading = 0
  var x = 0
  var y = 0
  val EW = "([EW])(\\d+)".r
  val NS = "([NS])(\\d+)".r
  val LR = "([LR])(\\d+)".r
  val F = "(F)(\\d+)".r
  val COUTER_CLOCKWISE = "(L90)|(R270)".r
  val CLOCKWISE = "(R90)|(L270)".r
  val REVERSE = "(L180)|(RL180)".r

  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle12.txt")).getLines) {
    line match {
      case EW(d, dx) => x += (if (d == "E") 1 else -1) * dx.toInt
      case NS(d, dy) => y += (if (d == "N") 1 else -1) * dy.toInt
      case LR(r, dh) => heading = (360 + heading + (if (r == "L") 1 else -1) * dh.toInt) % 360
      case F(_, f) => heading match {
        case 0 => x += f.toInt
        case 90 => y += f.toInt
        case 180 => x -= f.toInt
        case 270 => y -= f.toInt
      }
    }
  }
  println(s"Answer part 1: ${Math.abs(x) + Math.abs(y)}")

  x = 0
  y = 0
  var wx = 10
  var wy = 1
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle12.txt")).getLines) {
    line match {
      case EW(d, dx) => wx += (if (d == "E") 1 else -1) * dx.toInt;
      case NS(d, dy) => wy += (if ( d == "N") 1 else -1) * dy.toInt
      case COUTER_CLOCKWISE() =>
          val temp = wy
          wy = wx
          wx = -temp
      case CLOCKWISE() =>
          val temp = wy
          wy = -wx
          wx = temp
      case REVERSE() =>
          wx = -wx
          wy = -wy
      case F(_, f) =>
        x += wx * f.toInt
        y += wy * f.toInt
    }
  }
  println(s"Answer part 2: ${Math.abs(x) + Math.abs(y)}")

}
