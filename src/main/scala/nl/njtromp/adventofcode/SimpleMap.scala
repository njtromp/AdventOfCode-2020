package nl.njtromp.adventofcode

import nl.njtromp.adventofcode

import scala.reflect.ClassTag

trait SimpleMapTypes {
  //           (y,   x)
  type Delta = (Int, Int)
  type Pos =   (Int, Int)

  val up: Delta = (-1, 0)
  val down: Delta = (1, 0)
  val left: Delta = (0, -1)
  val right: Delta = (0, 1)
  val upLeft: Delta = (-1, -1)
  val upRight: Delta = (-1, 1)
  val downLeft: Delta = (1, -1)
  val downRight: Delta = (1, 1)
  val vertical: List[Delta] = List(up, down)
  val horizontal: List[Delta] = List(left, right)
  val square: List[Delta] = vertical ++ horizontal
  val diagonalCW: List[Delta] = List(upRight, downLeft)
  val diagonalCCW: List[Delta] = List(upLeft, downRight)
  val diagonal: List[Delta] = diagonalCW ++ diagonalCCW
  val all: List[Delta] = square ++ diagonal
}

class SimpleMap[A](elems: Array[Array[A]]) extends SimpleMapTypes {
  val height: Int = elems.length
  val width: Int = elems(0).length
  def apply(p: Pos): A = elems(p._1)(p._2)
  def isOnMap(p: Pos): Boolean = elems.indices.contains(p._1) && elems(0).indices.contains(p._2)
  def row(y: Int): List[A] = elems(y).toList
  def column(x: Int): List[A] = elems(0).indices.map(elems(_)(x)).toList
  def neighbors(p: Pos, directions: List[Delta]): List[A] =
    directions.map(d => (p._1 + d._1, p._2 + d._2))
      .filter(p => isOnMap(p))
      .map(this(_))
  def allNeighbors(p: Pos, directions: List[Delta]): List[A] =
    (1 to Math.max(height, width)).flatMap(l => directions.map(d => (p._1 + d._1 * l, p._2 + d._2 * l)))
      .filter(p => isOnMap(p))
      .map(this(_)).toList
}

object SimpleMap {
  def apply[A: ClassTag](lines: List[String], mapping: String => Array[A]): SimpleMap[A] =
    new SimpleMap(lines.map(l => mapping(l)).toArray)
}
