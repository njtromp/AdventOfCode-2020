package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.StringPuzzle

import scala.annotation.tailrec

class Day13 extends StringPuzzle {

  private val DIRECTIONS = "^>v<"
  private val CRASHED = -1
  private val UP = 0
  private val RIGHT = 1
  private val DOWN = 2
  private val LEFT = 3
  private val DELTAS = Array((0, -1), (1, 0), (0, 1), (-1, 0))

  private class Cart(var x: Int, var y: Int, var direction: Int){
    var turn = 0
    def move(tracks: Array[Array[Char]], carts: List[Cart]): Unit = {
      if (direction != CRASHED) {
        val crashes = carts.filter(_.direction != CRASHED).groupBy(c => (c.x, c.y)).filter(_._2.size > 1)
        if (crashes.isEmpty) {
          x += DELTAS(direction)._1
          y += DELTAS(direction)._2
          tracks(y)(x) match {
            case '+' => turnOnIntersection()
            case '/' => direction = direction match {
              case UP => RIGHT
              case RIGHT => UP
              case DOWN => LEFT
              case LEFT => DOWN
            }
            case '\\' => direction = direction match {
              case UP => LEFT
              case RIGHT => DOWN
              case DOWN => RIGHT
              case LEFT => UP
            }
            case _ =>
          }
        }
      }
    }
    def turnOnIntersection(): Unit = {
      turn match {
        case 0 => direction = (direction - 1 + 4) % 4
        case 2 => direction = (direction + 1) % 4
        case _ =>
      }
      turn = (turn + 1) % 3
    }
    override def toString: String = s"$x,$y ${if (direction == CRASHED) 'X' else DIRECTIONS(direction)}"
  }

  private def findCarts(tracks: Array[Array[Char]]): List[Cart] = {
    val cartLocations = tracks.indices.flatMap(y => tracks(y).indices.filter(x => DIRECTIONS.contains(tracks(y)(x))).map((_, y)))
    val carts = cartLocations.map(loc => new Cart(loc._1, loc._2, DIRECTIONS.indexOf(tracks(loc._2)(loc._1))))
    carts.foreach(c => tracks(c.y)(c.x) = if (c.direction % 2 == 0) '|' else '-')
    carts.toList
  }

  @tailrec
  private def moveCarts(tracks: Array[Array[Char]], carts: List[Cart]): (Int, Int) = {
    carts.sortBy(c => c.y * 1000 + c.x).foreach(_.move(tracks, carts))
    val crashes = carts.groupBy(c => (c.x, c.y)).filter(_._2.size > 1)
    if (crashes.nonEmpty) {
      (crashes.keys.head._1, crashes.keys.head._2)
    } else
      moveCarts(tracks, carts)
  }

  @tailrec
  private def removeCrashedCarts(tracks: Array[Array[Char]], carts: List[Cart]): (Int, Int) = {
    carts.filter(_.direction != CRASHED).sortBy(c => c.y * 1000 + c.x).foreach(c => {
      c.move(tracks, carts)
      // In case there was a crash, register it
      carts.filter(_.direction != CRASHED).groupBy(c => (c.x, c.y)).filter(_._2.size > 1).foreach(_._2.foreach(_.direction = CRASHED))
    })
    val activeCarts = carts.filter(_.direction != CRASHED)
    if (activeCarts.size == 1)
      (activeCarts.head.x, activeCarts.head.y)
    else if (activeCarts.isEmpty) // Keep the tracks from part 1 working with part 2
      (6, 4)
    else
      removeCrashedCarts(tracks, carts)
  }

  override def exampleAnswerPart1: String = "7,3"
  override def solvePart1(lines: List[String]): String = {
    val tracks = lines.map(_.toArray).toArray
    val carts = findCarts(tracks)
    val crash = moveCarts(tracks, carts)
    s"${crash._1},${crash._2}"
  }

  override def exampleAnswerPart2: String = "6,4"
  override def solvePart2(lines: List[String]): String = {
    val tracks = lines.map(_.toArray).toArray
    val carts = findCarts(tracks)
    val survivor = removeCrashedCarts(tracks, carts)
    s"${survivor._1},${survivor._2}"
  }

  private def printTracks(tracks: Array[Array[Char]], carts: List[Cart]): Unit = {
    tracks.indices.foreach(y => {
      tracks(y).indices.foreach(x => {
        val cart = carts.find(c => c.x == x && c.y == y)
        print(if (cart.isDefined) DIRECTIONS(cart.get.direction) else tracks(y)(x))
      })
      println
    })
  }

}

object Day13 extends App {
  new Day13().solvePuzzles("/2018/day13.txt")
}
