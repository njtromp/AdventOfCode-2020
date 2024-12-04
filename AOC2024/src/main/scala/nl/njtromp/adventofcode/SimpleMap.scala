package nl.njtromp.adventofcode

import scala.reflect.ClassTag

trait SimpleMapTypes {
  //           (y,   x)
  type Delta = (Int, Int)
  type Pos =   (Int, Int)

  val UP: Delta = (-1, 0)
  val DOWN: Delta = (1, 0)
  val LEFT: Delta = (0, -1)
  val RIGHT: Delta = (0, 1)
  val UP_LEFT: Delta = (-1, -1)
  val UP_RIGHT: Delta = (-1, 1)
  val DOWN_LEFT: Delta = (1, -1)
  val DOWN_RIGHT: Delta = (1, 1)
  val VERTICAL: List[Delta] = List(UP, DOWN)
  val HORIZONTAL: List[Delta] = List(LEFT, RIGHT)
  val SQUARE: List[Delta] = VERTICAL ++ HORIZONTAL
  val DIAGONAL_CW: List[Delta] = List(UP_RIGHT, DOWN_LEFT)
  val DIAGONAL_CCW: List[Delta] = List(UP_LEFT, DOWN_RIGHT)
  val DIAGONAL: List[Delta] = DIAGONAL_CW ++ DIAGONAL_CCW
  val ALL_DIRECTIONS: List[Delta] = SQUARE ++ DIAGONAL
}

class SimpleMap[A](val elems: Array[Array[A]]) extends SimpleMapTypes {
  val height: Int = elems.length
  val width: Int = elems.map(_.length).max
  def apply(p: Pos): A = elems(p._1)(p._2)
  def update(p: Pos, v: A): Unit = elems(p._1)(p._2) = v
  def isOnMap(p: Pos): Boolean = elems.indices.contains(p._1) && elems(0).indices.contains(p._2)
  def row(y: Int): List[A] = elems(y).toList
  def rows(): List[String] = elems.map(_.mkString).toList
  def column(x: Int): List[A] = (0 until height).map(elems(_)(x)).toList
  def columns(): List[String] = (0 until width).map(column(_).mkString).toList
  def move(p: Pos, d: Delta): Pos = (p._1 + d._1, p._2 + d._2)
  def move(p: Pos, d: Delta, l: Int): Pos = (p._1 + l * d._1, p._2 + l * d._2)
  def moveOpposite(p: Pos, d: Delta): Pos = (p._1 - d._1, p._2 - d._2)
  def moveOpposite(p: Pos, d: Delta, l: Int): Pos = (p._1 - l * d._1, p._2 - l *d._2)
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
  def find(predicate: A => Boolean): List[Pos] =
    allPositions().filter(p => predicate(this(p)))
  def getElements(p: Pos, d: Delta, l: Int): List[A] =
    if !isOnMap(p) || l == 0 then
      Nil
    else
      apply(p) :: getElements(move(p, d), d, l - 1)
  def asString(): String = elems.map(_.mkString).mkString("\n")
}

object SimpleMap {
  def apply[A: ClassTag](lines: List[String], mapping: String => Array[A]): SimpleMap[A] =
    new SimpleMap(lines.map(l => mapping(l)).toArray)
}
