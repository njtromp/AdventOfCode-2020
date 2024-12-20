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

  extension (p: Pos)
    def -(o: Pos): Delta = (o._1 - p._1, o._2 - p._2)
    def +(d: Delta): Pos = (p._1 + d._1, p._2 + d._2)
    def ==(o: Pos): Boolean = p._1 == o._1 && p._2 == o._2
  
  extension (d: Delta)
    def *(l: Int): Pos = (l * d._1, l * d._2)
    def manhattan: Int = Math.abs(d._1) + Math.abs(d._2)
    def opposite: Delta = d - (0, 0)

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
  def move(p: Pos, d: Delta): Pos = p + d
  def move(p: Pos, d: Delta, l: Int): Pos = p + (d * l)
  def moveOpposite(p: Pos, d: Delta): Pos = d - p
  def moveOpposite(p: Pos, d: Delta, l: Int): Pos = (d * l) - p
  def allPositions(): List[Pos] =
    (0 until height).flatMap(y => {
      (0 until width).map(x => (y, x))
    }).toList
  def manhattanNeighborPositions(p: Pos, distance: Int): List[Pos] =
    (-distance to distance).flatMap(y =>
      (-(distance - Math.abs(y)) to distance - Math.abs(y)).map(x => p + (y, x))
    ).filter(isOnMap).toList
  def manhattanNeighbors(p: Pos, distance: Int): List[A] =
    manhattanNeighborPositions(p, distance)
      .map(this(_))
  def neighborPositions(p: Pos, directions: List[Delta]): List[Pos] =
    directions.map(p + _)
      .filter(isOnMap)
  def neighbors(p: Pos, directions: List[Delta]): List[A] =
    neighborPositions(p, directions).map(this(_))
  def allNeighborPositions(p: Pos, directions: List[Delta]): List[Pos] =
    (1 to Math.max(height, width)).flatMap(l => directions.map(d => move(p, d, l)))
      .filter(isOnMap).toList
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
  def apply(lines: List[String]): SimpleMap[Char] = apply(lines, _.toCharArray)
  def apply[A: ClassTag](lines: List[String], mapping: String => Array[A]): SimpleMap[A] =
    new SimpleMap(lines.map(l => mapping(l)).toArray)
}
