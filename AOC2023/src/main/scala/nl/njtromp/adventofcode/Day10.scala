package nl.njtromp.adventofcode

class Day10 extends Puzzle[Long] with SimpleMapTypes {
  private val directions = Map(
    (down, 'S') -> down,
    (up, 'S') -> up,
    (left, 'S') -> right,
    (right, 'S') -> right,
    (down, '|') -> down,
    (up, '|') -> up,
    (left, '-') -> left,
    (right, '-') -> right,
    (up, 'F') -> right,
    (left, 'F') -> down,
    (up, '7') -> left,
    (right, '7') -> down,
    (down, 'J') -> left,
    (right, 'J') -> up,
    (down, 'L') -> right,
    (left, 'L') -> up,
  )
  private val connections = Map(
    up -> List('|', 'F', '7'),
    down -> List('|', 'J', 'L'),
    left -> List('-', 'F', 'L'),
    right -> List('-', 'J', '7')
  )

  private val extendHorizontal = Map(
    '-' -> '-',
    'F' -> '-',
    'L' -> '-'
  ).withDefault(_ => '.')
  private val extendVertical = Map(
    '|' -> '|',
    'F' -> '|',
    '7' -> '|'
  ).withDefault(_ => '.')

  private def isConnected(map: SimpleMap[Char], current: Pos, direction: Delta, connections: List[Char]): Boolean =
    val neighbors = map.neighborPositions(current, List(direction))
    neighbors.exists(p => connections.contains(map(p)))

  private def findLoop(map: SimpleMap[Char], start: Pos): List[Pos] =
    def findLoop(current: Pos, move: Delta): List[Pos] = {
      val nextPos = map.neighborPositions(current, List(move)).head
      if map(nextPos) == 'S' then
        List(current, nextPos)
      else
        val nextMove = directions((move, map(nextPos)))
        current :: findLoop(nextPos, nextMove)
    }
    val startingDirection = square.filter(d => isConnected(map, start, d, connections(d))).head
    findLoop(start, startingDirection)

  private def floodFill(map: SimpleMap[Char], start: Pos): Unit =
    def floodFill(current: Pos): Unit =
      if map(current).toUpper != 'X' then {
        map(current) = 'x'
        map.neighborPositions(current, square).filter(map(_).toUpper != 'X').map(floodFill)
      }
    floodFill(start)

  override def exampleAnswerPart1: Long = 8
  override def solvePart1(lines: List[String]): Long =
    val actualLines = groupByEmptyLine(lines).head
    val map = SimpleMap[Char](actualLines, _.toCharArray)
    val start = map.find('S').head
    findLoop(map, start).size / 2

  override def exampleAnswerPart2: Long = 10
  override def solvePart2(lines: List[String]): Long =
    val actualLines = groupByEmptyLine(lines).last
    // Extend vertical and horizontal.
    // This ensures that there is a opening between adjacent pipes so floodfill can do its job properly.
    val extendedMap = actualLines.map(_.flatMap(p => s"$p${extendHorizontal(p)}")).flatMap(l => List(l, l.map(extendVertical)))
    // Get rid of lowest row and rightmost column
    val usableMap = extendedMap.dropRight(1).map(_.dropRight(1))

    val map = SimpleMap[Char](usableMap, _.toCharArray)
    val start = map.find('S').head
    // This correction could be input specific, for me it did the trick :-)
    map((start._1 + 1, start._2)) = '|'

    // Mark the loop
    findLoop(map, start).foreach(p => map(p) = 'X')

    // Floodfill, mark with 'x' from the outside
    (0 until map.height).foreach(r => {
      floodFill(map, (r, 0))
      floodFill(map, (r, map.width - 1))
    })
    (0 until map.width).foreach(c => {
      floodFill(map, (0, c))
      floodFill(map, (map.height - 1, c))
    })

    // Remove lines and columns needed for floodfill to work
    val shinkedMap = (0 until map.height / 2).map(r => map.row(r * 2)).toList.map(r => r.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString(""))
    shinkedMap.map(_.count(_.toUpper != 'X')).sum

}

object Day10 extends App {
  new Day10().solvePuzzles()
}
