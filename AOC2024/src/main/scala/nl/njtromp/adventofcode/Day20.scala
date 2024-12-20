package nl.njtromp.adventofcode

class Day20 extends Puzzle[Long] with RouteFinding {

  private def findCheats(path: List[Pos], cheatSize: Int, map: SimpleMap[Char]): List[((Pos, Pos), Long)] =
    def cheatCandidates(path: List[Pos]): List[((Pos, Pos), Long)] =
      if path.length <= 4 then
        List.empty
      else
        val current = path.head
        val candidates = (-(cheatSize + 1) to cheatSize + 1).flatMap(y =>
          (-(cheatSize + 1) to cheatSize + 1).map(x => current + (y, x))
        ).filter(p => (p - current).manhattan <= cheatSize)
          .filter(map.isOnMap)
          .filter(map(_) == '.').toList
        // Make sure the candidates are closer to the finish, hence dropping the first 4 positions on the path
        val cheats = candidates.filter(path.drop(4).contains)
        cheats.map(p => ((current, p), path.indexOf(p).toLong - (p - current).manhattan)) ++ cheatCandidates(path.tail)
    cheatCandidates(path)

  // Using the examples that are greater or equal 10
  override def exampleAnswerPart1: Long = 2 + 3 + 1 + 1 + 1 + 1 + 1
  override def solvePart1(lines: List[String]): Long =
    // Needed for reconstructing the path
    println("Run with at least 8M of stack-space!: -Xss8M")
    val map = SimpleMap(lines)
    val start = map.find('S').head
    val finish = map.find('E').head
    map(finish) = '.' // Make sure we can find the finish
    val path = dijkstra(start, finish, map.neighborPositions(_, SQUARE).filter(map(_) == '.'), (_, _) => 1)
    val cheats = findCheats(path, 2, map)
    if map.height == 15 then cheats.count(_._2 >= 10) else cheats.count(_._2 >= 100)

  // Using the examples that are greater or equal 70
  override def exampleAnswerPart2: Long = 12 + 22 + 4 + 3
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines)
    val start = map.find('S').head
    val finish = map.find('E').head
    map(finish) = '.' // Make sure we can find the finish
    val path = dijkstra(start, finish, map.neighborPositions(_, SQUARE).filter(map(_) == '.'), (_, _) => 1)
    val cheats = findCheats(path, 20, map)
    val result = if map.height == 15 then cheats.count(_._2 >= 70) else cheats.count(_._2 >= 100)
    result

}

object Day20 extends App {
  new Day20().solvePuzzles()
}
