package nl.njtromp.adventofcode

class Day23 extends Puzzle[Long] with RouteFinding {

  def findRoute[A](map: SimpleMap[A], canReach: (Pos, Pos) => Boolean, start: Pos, finish: Pos): List[Pos] =
    var maxDistance: Long = 0L
    var maxPath: List[Pos] = List.empty
    def findRoute(current: Pos, distance: Long, path: List[Pos], visited: Set[Pos]): Unit =
      if current == finish then
        if distance > maxDistance then
          maxDistance = distance
          maxPath = path
      else
        map.neighborPositions(current, square)
          .filter(n => canReach(current, n) && !visited.contains(n))
          .foreach(n =>
            findRoute(n, distance + 1, n :: path, visited + n)
          )
    findRoute(start, 0, List(start), Set(start))
    maxPath

  override def exampleAnswerPart1: Long = 94
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    def canReach(c: Pos, t: Pos): Boolean =
      map(c) match {
        case '<' => t._2 < c._2 && map(t) != '#'
        case '>' => t._2 > c._2 && map(t) != '#'
        case '^' => t._1 < c._1 && map(t) != '#'
        case 'v' => t._1 > c._1 && map(t) != '#'
        case _ => map(t) != '#'
      }
    val start = (0, lines.head.indexOf('.'))
    val finish = (map.height - 1, lines.last.indexOf('.'))
    findRoute(map, canReach, start, finish).size - 1

  override def exampleAnswerPart2: Long = 154
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    def canReach(c: Pos, t: Pos): Boolean =
      map(t) != '#'
    val start = (0, lines.head.indexOf('.'))
    val finish = (map.height - 1, lines.last.indexOf('.'))
    findRoute(map, canReach, start, finish).size - 1

}

object Day23 extends App {
  println("Please run with -Xss128M")
  new Day23().solvePuzzles()
}
