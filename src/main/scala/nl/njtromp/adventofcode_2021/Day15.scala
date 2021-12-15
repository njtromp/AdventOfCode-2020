package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day15 extends Puzzle {

  type Pos = (Int, Int) // (X, Y)
  private def x(p: Pos) = p._1
  private def y(p: Pos) = p._2

  override def solvePart1(lines: List[String]): Long = {
    val riskMap = lines.map(_.toArray.map(_.asDigit)).toArray
    val from = riskMap.map(_ => new Array[Pos](riskMap.head.length))
    val weights = riskMap.map(_.map(_ => Long.MaxValue))
    val moves = List((-1, 0), (1, 0), (0, -1), (0, 1))

    def move(p: Pos, d: Pos): Pos = (p._1 + d._1, p._2 + d._2)
    def onMap(p: Pos): Boolean = x(p) >= 0 && x(p) < riskMap.head.length && y(p) >= 0 && y(p) < riskMap.length
    @tailrec
    def findPath(visited: Set[Pos], queue: Set[WeightedPos]): Unit = {
      if (queue.nonEmpty) {
        val wp = queue.min(WeightedPosOrdering)
        val newNodes = moves.map(move(wp.pos, _)).filter(onMap).filterNot(visited.contains).foldLeft(List.empty[WeightedPos])((newNodes, p) => {
          val lowerWeight = wp.weight + riskMap(y(p))(x(p))
          if (lowerWeight < weights(y(p))(x(p))) {
            weights(y(p))(x(p)) = lowerWeight
            from(y(p))(x(p)) = wp.pos
          }
          WeightedPos(lowerWeight, p) :: newNodes
        })
        findPath(visited + wp.pos, queue.filterNot(_.pos == wp.pos) ++ newNodes)
      }
    }
    weights(0)(0) = 0
    findPath(Set.empty[Pos], Set(WeightedPos(0, (0,0))))
    weights.reverse.head.reverse.head
  }

  override def solvePart2(lines: List[String]): Long = {
    def enlarge(m: Array[Array[Int]]): Array[Array[Int]] = m.map(_.map(n => Math.max(1, (n + 1) % 10)))
    def expandeHorizontal(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = m1.zip(m2).map(rs => rs._1 ++ rs._2)
    val riskMap = lines.map(_.toArray.map(_.asDigit)).toArray
    val verticalRiskMap = (1 until 5).foldLeft((riskMap, riskMap))((acc, n) => {
      val newTile = enlarge(acc._2)
      (acc._1 ++ newTile, newTile)
    })._1
    val expandedRiskMap = (1 until 5).foldLeft((verticalRiskMap, verticalRiskMap))((acc, n) => {
      val tileRow = enlarge(acc._2)
      (expandeHorizontal(acc._1, tileRow), tileRow)
    })._1
    solvePart1(expandedRiskMap.map(_.mkString).toList)
  }
}

case class WeightedPos(weight: Long, pos: (Int, Int))

object WeightedPosOrdering extends Ordering[WeightedPos] {
  override def compare(x: WeightedPos, y: WeightedPos): Int = (x.weight - y.weight).toInt
}

object Day15 extends App {
  new Day15().solvePuzzles("/2021/day15.txt")
}
