package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day15 extends Puzzle[Long] with SimpleMapTypes {

  private val FREE = '.'
  private val ROBOT = '@'
  private val WALL = '#'
  private val BOX = 'O'
  private val BOX_LEFT_SIDE = '['
  private val BOX_RIGHT_SIDE = ']'
  private val TRANSLATE = Map('^' -> UP, '>' -> RIGHT, 'v' -> DOWN, '<' -> LEFT)

  private case class Box(left: Pos, right: Pos) {
    def gpsCoordinate: Long =
      100 * left._1 + left._2
    def isAt(pos: Pos): Boolean =
      left == pos || right == pos
    def connectedBoxes(move: Delta, boxes: List[Box]): List[Box] =
      val pushedBoxes = boxes.filterNot(_ == this).filter(b => b.isAt(left + move) || b.isAt(right + move))
      pushedBoxes ++ pushedBoxes.flatMap(_.connectedBoxes(move, boxes))
    def canMove(move: Delta, walls: Set[Pos]): Boolean =
      !walls.contains(left + move) && !walls.contains(right + move)
    def move(move: Delta): Box =
      Box(left + move, right + move)
  }

  @tailrec
  private def moveRobot(robot: Box, moves: String, boxes: List[Box], walls: Set[(Int, Int)]): List[Box] =
    if moves.isEmpty then
      boxes
    else
      val move = TRANSLATE(moves.head)
      val boxesToMove = robot.connectedBoxes(move, boxes).toSet
      val canMove = !walls.contains(robot.left + move) && boxesToMove.forall(_.canMove(move, walls))
      if canMove then
        val newBoxes = boxesToMove.map(_.move(move))
        moveRobot(robot.move(move), moves.tail, boxes.filterNot(boxesToMove.contains) ++ newBoxes, walls)
      else
        moveRobot(robot, moves.tail, boxes, walls)

  private def enlarge(map: List[String]): List[String] =
    def enlarge(c: Char): String =
      c match
        case FREE => s"$FREE$FREE"
        case WALL => s"$WALL$WALL"
        case ROBOT => s"$ROBOT$FREE"
        case BOX => s"$BOX_LEFT_SIDE$BOX_RIGHT_SIDE"
    map.map(line => line.map(enlarge).mkString)

  override def exampleAnswerPart1: Long = 10092
  override def solvePart1(lines: List[String]): Long =
    val input = groupByEmptyLine(lines)
    val moves = input.last.mkString
    val map = SimpleMap(input.head)
    val robot = map.find(ROBOT).map(r => Box(r, r)).head
    val boxes = map.find(BOX).map(p => Box(p, p))
    val walls = map.find(WALL).toSet
    moveRobot(robot, moves, boxes, walls)
      .map(_.gpsCoordinate).sum

  override def exampleAnswerPart2: Long = 9021
  override def solvePart2(lines: List[String]): Long =
    val input = groupByEmptyLine(lines)
    val moves = input.last.mkString
    val map = SimpleMap(enlarge(input.head))
    val robot = map.find(ROBOT).map(r => Box(r, r)).head
    val boxes = map.find(BOX_LEFT_SIDE).map(p => Box(p, p + RIGHT))
    val walls = map.find(WALL).toSet
    moveRobot(robot, moves, boxes, walls)
      .map(_.gpsCoordinate).sum
}

object Day15 extends App {
  new Day15().solvePuzzles()
}
