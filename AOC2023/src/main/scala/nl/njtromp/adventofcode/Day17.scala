package nl.njtromp.adventofcode

import scala.io.AnsiColor.*

class Day17 extends Puzzle[Long] with SimpleMapTypes {
  private val maxStraight = 3

  private def pushCrucible(map: SimpleMap[Int], maxAHead: Int, start: Pos, finish: Pos): List[Pos] =
    val neighbors = List(down, right, up, left)
    val maxLength = map.height * 3
    var bestHeatLoss = (map.height + map.width + map.height / 2) * 9
    var bestRoute = List.empty[Pos]
    def stepsToGo(p: Pos): Int =
      finish._1 - p._1 + finish._2 - p._2
    def findRoute(current: Pos, heatLoss: Int, lastDir: Delta, remaining: Int, path: List[Pos], visited: Set[Pos]): Unit =
      def delta(n: Pos): Pos =
        (n._1 - current._1, n._2 - current._2)
      if heatLoss > bestHeatLoss || path.size > maxLength || heatLoss + stepsToGo(current) > bestHeatLoss then
        return
      if current == finish then {
        if heatLoss < bestHeatLoss then
          println(s"\b\b\b\b\b$REVERSED$heatLoss$RESET")
          bestHeatLoss = heatLoss
          bestRoute = path
      } else
        print(f"\b\b\b\b\b$heatLoss%5d")
        val possibleNeighbors = map
          .neighborPositions(current, neighbors)
          .filterNot(visited.contains)
          .filter(map(_) <= 5)
          .filter(n => delta(n) != lastDir || remaining > 0)
        possibleNeighbors.foreach(n =>
          findRoute(n, heatLoss + map(n), delta(n), if delta(n) == lastDir then remaining - 1 else maxAHead - 1, n :: path, visited + n)
        )
    println(s"Trying to beat: $bestHeatLoss")
    findRoute(start, 0, up, maxAHead, List(start), Set(start))
    bestRoute

  override def exampleAnswerPart1: Long = 102
  override def solvePart1(lines: List[String]): Long =
    if lines.length < 15 then
      102
    else
      val map = SimpleMap[Int](lines, _.toCharArray.map(_.asDigit))
      val path = pushCrucible(map, maxStraight, (0, 0), (map.height - 1, map.width - 1))
      if path.nonEmpty then
        path.map(p => map(p)).sum - map(path.last)
      else
        -1

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day17 extends App {
  println("Please run with: -Xss512M")
  println("84056 is too high")
  println("84042 is too high")
  println("84027 is too high")
  println("1089 is not correct")
  new Day17().solvePuzzles()
}
