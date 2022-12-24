package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day22 extends Puzzle2 {
  type Pos = (Int, Int)
  type Wrap = (Int, Int, Int, Array[Array[Char]]) => (Int, Int, Int)

  private val RIGHT = 0
  private val DOWN = 1
  private val LEFT = 2
  private val UP = 3

  private var y: Int = -1
  private var x: Int = -1
  private var facing = RIGHT

  private val DIRECTIONS = Array("right", "down", "left", "up")

  private def extend(line: String, length: Int): String = {
    val padding = " "*(length - line.length)
    s"$line$padding"
  }

  private def centralize(pos: Pos, size: Int): Pos = {
    val half = size / 2
    (pos._1 < half, pos._2 < half ) match {
      case (true, true) => (pos._1 - half, pos._2 - half)
      case (true, false) => (pos._1 - half, pos._2 - half + 1)
      case (false, true) => (pos._1 - half + 1, pos._2 - half)
      case (false, false) => (pos._1 - half + 1, pos._2 - half + 1)
    }
  }
  private def decentralize(pos: Pos, size: Int): Pos = {
    val half = size / 2
    (pos._1 < 0, pos._2 < 0) match {
      case (true, true) => (pos._1 + half, pos._2 + half)
      case (true, false) => (pos._1 + half, pos._2 + half - 1)
      case (false, true) => (pos._1 + half - 1, pos._2 + half)
      case (false, false) => (pos._1 + half - 1, pos._2 + half - 1)
    }

  }

  private def rotateClockwise(p: Pos, size: Int): Pos = {
    val c = centralize(p, size)
    decentralize((c._2, -c._1), size)
  }

  private def rotateCounterClockwise(p: Pos, size: Int): Pos = {
    val c = centralize(p, size)
    decentralize((-c._2, c._1), size)
  }

  private def transpose(direction: Int, amount: Int, pos: Pos): Pos =
    direction match {
      case RIGHT => (pos._1, pos._2 + amount)
      case LEFT => (pos._1, pos._2 - amount)
      case DOWN => (pos._1 + amount, pos._2)
      case UP => (pos._1 - amount, pos._2)
    }

  private def wrapMap(y: Int, x: Int, facing: Int, map: Array[Array[Char]]): (Int, Int, Int) = {
    var newX = x
    var newY = y
    if (facing == RIGHT) {
      newX = x + 1
      if (newX == map(y).length || map(y)(newX) == ' ')
        newX = Math.min(map(y).indexOf('.'), map(y).indexOf('#'))
    } else if (facing == LEFT) {
      newX = x - 1
      if (x == 0 || map(y)(newX) == ' ')
        newX = Math.max(map(y).lastIndexOf('.'), map(y).lastIndexOf('#'))
    } else if (facing == DOWN) {
      newY = y + 1
      if (newY == map.length || map(newY)(x) == ' ') {
        val row = map.map(_(x)).mkString
        newY = Math.min(row.indexOf('.'), row.indexOf('#'))
      }
    } else { // facing == UP
      newY = y - 1
      if (y == 0 || map(newY)(x) == ' ') {
        val row = map.map(_(x)).mkString
        newY = Math.max(row.lastIndexOf('.'), row.lastIndexOf('#'))
      }
    }
    if (map(newY)(newX) == '.')
      (newY, newX, facing)
    else
      (y, x, facing)
  }

  private def move(facing: Int, pos: Pos): Pos = {
    facing match {
      case LEFT => (pos._1, pos._2 - 1)
      case RIGHT => (pos._1, pos._2 + 1)
      case UP => (pos._1 - 1, pos._2)
      case DOWN => (pos._1 + 1, pos._2)
    }
  }

  private def isOnSquare(size: Int, pos: Pos): Boolean = pos._1 >= 0 && pos._1 < size && pos._2 >= 0 && pos._2 < size

  private def wrapCubeSmall(y: Int, x: Int, facing: Int, map: Array[Array[Char]]): (Int, Int, Int) = {
    val size = 4
    val zoneX = x / size
    val zoneY = y / size

    var newPos: Pos = (y, x)
    var newFacing = facing

    zoneX match {
      case 0 =>
        val pos = move(facing, transpose(UP, size, (y, x)))
        if (isOnSquare(size, pos))
          newPos = transpose(DOWN, size, pos)
        else facing match {
          case LEFT =>
            newPos = transpose(RIGHT, 3 * size, transpose(DOWN, 3 * size, rotateClockwise(pos, size)))
            newFacing = UP
          case RIGHT => newPos = transpose(DOWN, size, pos)
          case UP =>
            newPos = transpose(UP, size, transpose(RIGHT, 2 * size, rotateClockwise(rotateClockwise(pos, size), size)))
            newFacing = DOWN
          case DOWN =>
            newPos = transpose(RIGHT, 2 * size, transpose(DOWN, 3 * size, rotateClockwise(rotateClockwise(pos, size), size)))
            newFacing = UP
        }
      case 1 =>
        val pos = move(facing, transpose(LEFT, size, transpose(UP, size, (y, x))))
        if (isOnSquare(size, pos))
          newPos = transpose(RIGHT, size, transpose(DOWN, size, pos))
        else facing match {
          case LEFT => newPos = transpose(RIGHT, size, transpose(DOWN, size, pos))
          case RIGHT => newPos = transpose(RIGHT, size, transpose(DOWN, size, pos))
          case UP =>
            newPos = transpose(RIGHT, size, rotateClockwise(pos, size))
            newFacing = RIGHT
          case DOWN =>
            newPos = transpose(RIGHT, size, transpose(DOWN, 2 * size, rotateCounterClockwise(pos, size)))
            newFacing = RIGHT
        }
      case 2 => zoneY match {
        case 0 =>
          val pos = move(facing, transpose(LEFT, 2 * size, (y, x)))
          if (isOnSquare(size, pos))
            newPos = transpose(RIGHT, 2 * size, pos)
          else facing match {
            case LEFT =>
              newPos = transpose(RIGHT, size, rotateCounterClockwise(pos, size))
              newFacing = DOWN
            case RIGHT =>
              newPos = transpose(RIGHT, 4 * size, transpose(DOWN, 2 * size, rotateClockwise(rotateClockwise(pos, size), size)))
              newFacing = LEFT
            case UP =>
              newPos = transpose(RIGHT, size, rotateClockwise(rotateClockwise(pos, size), size))
              newFacing = LEFT
            case DOWN =>
              newPos = transpose(RIGHT, 2 * size, pos)
          }
        case 1 =>
          val pos = move(facing, transpose(LEFT, 2 * size, transpose(UP, size, (y, x))))
          if (isOnSquare(size, pos))
            newPos = transpose(RIGHT, 2 * size, transpose(DOWN, size, pos))
          else facing match {
            case LEFT => newPos = transpose(RIGHT, 2 * size, transpose(DOWN, size, pos))
            case RIGHT =>
              newPos = transpose(DOWN, size, transpose(RIGHT, 3 * size, rotateClockwise(pos, size)))
              newFacing = DOWN
            case UP => newPos = transpose(RIGHT, 2 * size, transpose(DOWN, size, pos))
            case DOWN => newPos = transpose(RIGHT, 2 * size, transpose(DOWN, size, pos))
          }
        case 2 =>
          val pos = move(facing, transpose(LEFT, 2 * size, transpose(UP, 2 * size, (y, x))))
          if (isOnSquare(size, pos))
            newPos = transpose(RIGHT, 2 * size, transpose(DOWN, 2 * size, pos))
          else facing match {
            case LEFT => transpose(RIGHT, size, transpose(DOWN, 2 * size, rotateClockwise(pos, size)))
            case RIGHT => newPos = transpose(RIGHT, 2 * size, transpose(DOWN, 2 * size, pos))
            case UP => newPos = transpose(RIGHT, 2 * size, transpose(DOWN, 2 * size, pos))
            case DOWN =>
              newPos = transpose(DOWN, 2 * size, rotateClockwise(rotateClockwise(pos, size), size))
              newFacing = UP
          }
      }
      case 3 =>
        val pos = move(facing, transpose(LEFT, 3 * size, transpose(UP, 2 * size, (y, x))))
        if (isOnSquare(size, pos))
          newPos = transpose(RIGHT, 3 * size, transpose(DOWN, 2 * size, pos))
        else facing match {
          case LEFT => newPos = transpose(RIGHT, 3 * size, transpose(DOWN, 2 * size, pos))
          case RIGHT =>
            newPos = transpose(RIGHT, 3 * size, rotateClockwise(rotateClockwise(pos, size), size))
            newFacing = LEFT
          case UP =>
            transpose(RIGHT, 3 * size, transpose(DOWN, size, rotateCounterClockwise(pos, size)))
            newFacing = LEFT
          case DOWN =>
            newPos = transpose(LEFT, size, transpose(DOWN, size, rotateCounterClockwise(pos, size)))
            newFacing = RIGHT
        }
    }

    // CHECK MOVE
    if (map(newPos._1)(newPos._2) == '.') {
      (newPos._1, newPos._2, newFacing)
    } else
      (y, x, facing)
  }

  private def wrapCubeLarge(y: Int, x: Int, facing: Int, map: Array[Array[Char]]): (Int, Int, Int) = {
    val size = 50
    val zoneX = x / size
    val zoneY = y / size

    var newPos: Pos = (y, x)
    var newFacing = facing

    zoneY match {
      case 0 =>
        zoneX match {
          case 1 =>
            val pos = move(facing, transpose(LEFT, size, (y, x)))
            if (isOnSquare(size, pos))
              newPos = transpose(RIGHT, size, pos)
            else facing match {
              case LEFT =>
                newPos = transpose(LEFT, size, transpose(DOWN, 2 * size, rotateClockwise(rotateClockwise(pos, size), size)))
                newFacing = RIGHT
              case RIGHT => newPos = transpose(RIGHT, size, pos)
              case UP =>
                newPos = transpose(LEFT, size, transpose(DOWN, 3 * size, rotateClockwise(pos, size)))
                newFacing = RIGHT
              case DOWN => newPos = transpose(RIGHT, size, pos)
            }
          case 2 =>
            val pos = move(facing, transpose(LEFT, 2 * size, (y, x)))
            if (isOnSquare(size, pos))
              newPos = transpose(RIGHT, 2 * size, pos)
            else facing match {
              case LEFT => newPos = transpose(RIGHT, 2 * size, pos)
              case RIGHT =>
                newPos = transpose(DOWN, 2 * size, transpose(RIGHT, 2 * size, rotateClockwise(rotateClockwise(pos, size), size)))
                newFacing = LEFT
              case UP =>
                newPos = transpose(DOWN, 4 * size, pos)
              case DOWN =>
                newPos = transpose(DOWN, size, transpose(RIGHT, 2 * size, rotateClockwise(pos, size)))
                newFacing = LEFT
            }
        }
      case 1 =>
        val pos = move(facing, transpose(LEFT, size, transpose(UP, size, (y, x))))
        if (isOnSquare(size, pos))
          newPos = transpose(RIGHT, size, transpose(DOWN, size, pos))
        else facing match {
          case LEFT =>
            newPos = transpose(DOWN, size, rotateCounterClockwise(pos, size))
            newFacing = DOWN
          case RIGHT =>
            newPos = transpose(RIGHT, 2 * size, transpose(DOWN, size, rotateCounterClockwise(pos, size)))
            newFacing = UP
          case UP => newPos = transpose(RIGHT, size, transpose(DOWN, size, pos))
          case DOWN => newPos = transpose(RIGHT, size, transpose(DOWN, size, pos))
        }
      case 2 =>
        zoneX  match {
          case 0 =>
            val pos = move(facing, transpose(UP, 2 * size, (y, x)))
            if (isOnSquare(size, pos))
              newPos = transpose(DOWN, 2 * size, pos)
            else facing match {
              case LEFT =>
                newPos = rotateClockwise(rotateClockwise(pos, size), size)
                newFacing = RIGHT
              case RIGHT => newPos = transpose(DOWN, 2 * size, pos)
              case UP =>
                newPos = transpose(DOWN, size, rotateClockwise(pos, size))
                newFacing = RIGHT
              case DOWN => newPos = transpose(DOWN, 2 * size, pos)
            }
          case 1 =>
            val pos = move(facing, transpose(LEFT, size, transpose(UP, 2 * size, (y, x))))
            if (isOnSquare(size, pos))
              newPos = transpose(RIGHT, size, transpose(DOWN, 2 * size, pos))
            else facing match {
              case LEFT => newPos = transpose(RIGHT, size, transpose(DOWN, 2 * size, pos))
              case RIGHT =>
                newPos = transpose(RIGHT, 3 * size, rotateClockwise(rotateClockwise(pos, size), size))
                newFacing =  LEFT
              case UP => newPos = transpose(RIGHT, size, transpose(DOWN, 2 * size, pos))
              case DOWN =>
                newPos = transpose(DOWN, 3 * size, transpose(RIGHT, size, rotateClockwise(pos, size)))
                newFacing = LEFT
            }
        }
      case 3 =>
        val pos = move(facing, transpose(UP, 3 * size, (y, x)))
        if (isOnSquare(size, pos))
          newPos = transpose(DOWN, 3 * size, pos)
        else facing match {
          case LEFT =>
            newPos = transpose(RIGHT, size, transpose(UP, size, rotateCounterClockwise(pos, size)))
            newFacing = DOWN
          case RIGHT =>
            newPos = transpose(RIGHT, size, transpose(DOWN, 3 * size, rotateCounterClockwise(pos, size)))
            newFacing = UP
          case UP => newPos = transpose(DOWN, 3 * size, pos)
          case DOWN =>
            newPos = transpose(UP, size, transpose(RIGHT, 2 * size, pos))
        }
    }

    // CHECK MOVE
    if (map(newPos._1)(newPos._2) == '.') {
      (newPos._1, newPos._2, newFacing)
    } else
      (y, x, facing)
  }

  @tailrec
  private def walk(path: String, map: Array[Array[Char]], wrap: Wrap): Unit = {
    @tailrec
    def move(steps: Int): Unit = {
      if (steps > 0) {
        wrap(y, x, facing, map) match {
          case (py, px, pf) =>
            y = py
            x = px
            facing = pf
        }
        move(steps - 1)
      }
    }

    if (path.nonEmpty) {
      val steps = path.takeWhile(_.isDigit)
      if (steps.nonEmpty) {
        move(steps.toInt)
        walk(path.drop(steps.length), map, wrap)
      } else {
        facing = if (path.head == 'R') (facing + 1) % 4 else (facing - 1 + 4) % 4
        walk(path.drop(1), map, wrap)
      }
    }
  }

  override def exampleAnswerPart1: Long = 6032
  override def solvePart1(lines: List[String]): Long = {
    val mapWidth = lines.filter(_.contains(".")).maxBy(_.length).length
    val map = lines.takeWhile(_.nonEmpty).map(extend(_, mapWidth).toArray).toArray
    y = 0
    x = map.head.indexOf('.')
    facing = RIGHT
    val path = lines.drop(map.length + 1).head
    walk(path, map, wrapMap)
    1000 * (y + 1) + 4 * (x + 1) + facing
  }

  override def exampleAnswerPart2: Long = 5031
  override def solvePart2(lines: List[String]): Long = {
    val mapWidth: Int = lines.filter(_.contains(".")).maxBy(_.length).length
    val map = lines.takeWhile(_.nonEmpty).map(extend(_, mapWidth).toArray).toArray
    val mapHeight: Int = map.length
    val squareSize: Int = Math.abs(mapHeight - mapWidth)
    y = 0
    x = map.head.indexOf('.')
    facing = RIGHT
    val path = lines.drop(map.length + 1).head
    if (squareSize == 4)
      walk(path, map, wrapCubeSmall)
    else
      walk(path, map, wrapCubeLarge)
    1000 * (y + 1) + 4 * (x + 1) + facing
  }

}

object Day22 extends App{
  new Day22().solvePuzzles("/2022/day22.txt")
}
