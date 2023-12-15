package nl.njtromp.adventofcode

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

class SimpleMap[A](val elems: Array[Array[A]]) extends SimpleMapTypes {
  val height: Int = elems.length
  val width: Int = elems(0).length
  def apply(p: Pos): A = elems(p._1)(p._2)
  def update(p: Pos, v: A): Unit = elems(p._1)(p._2) = v
  def isOnMap(p: Pos): Boolean = elems.indices.contains(p._1) && elems(0).indices.contains(p._2)
  def row(y: Int): List[A] = elems(y).toList
  def rows(): List[String] = elems.map(_.mkString).toList
  def column(x: Int): List[A] = (0 until height).map(elems(_)(x)).toList
  def columns(): List[String] = (0 until width).map(column(_).mkString).toList
  def allPositions(): List[Pos] =
    (0 until height).flatMap(y => {
      (0 until width).map(x => (y, x))
    }).toList
  def neighborPositions(p: Pos, directions: List[Delta]): List[Pos] =
    directions.map(d => (p._1 + d._1, p._2 + d._2))
      .filter(p => isOnMap(p))
  def neighbors(p: Pos, directions: List[Delta]): List[A] =
    neighborPositions(p, directions).map(this(_))
  def allNeighborPositions(p: Pos, directions: List[Delta]): List[Pos] =
    (1 to Math.max(height, width)).flatMap(l => directions.map(d => (p._1 + d._1 * l, p._2 + d._2 * l)))
      .filter(p => isOnMap(p)).toList
  def allNeighbors(p: Pos, directions: List[Delta]): List[A] =
      allNeighborPositions(p, directions).map(this(_))
  def find(item: A): List[Pos] =
    allPositions().filter(p => this(p) == item)
  def asString(): String = elems.map(_.mkString).mkString("\n")
}

object SimpleMap {
  def apply[A: ClassTag](lines: List[String], mapping: String => Array[A]): SimpleMap[A] =
    new SimpleMap(lines.map(l => mapping(l)).toArray)
}
