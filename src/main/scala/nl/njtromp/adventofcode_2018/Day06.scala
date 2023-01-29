package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

class Day06 extends Puzzle2 {

  private type Point = (Int, Int)

  private def toLocation(line: String): Point = {
    val parts = line.split(",").map(_.trim.toInt)
    (parts(0), parts(1))
  }

  private def distance(a: Point, b: Point): Int = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  private def mapAreas(locations: List[Point]): Array[Array[Int]] = {
    val minX = locations.minBy(_._1)._1
    val maxX = locations.maxBy(_._1)._1
    val minY = locations.minBy(_._2)._2
    val maxY = locations.maxBy(_._2)._2
    val height = maxY - minY + 1
    val width = maxX - minX + 1
    val map = Array.ofDim[Int](height, width)
    val ids = locations.zipWithIndex.toMap.view.mapValues(_ + 1)
    (minY to maxY).foreach(y =>
      (minX to maxX).foreach(x => {
        val distances = locations.map(l => (distance(l, (x, y)), l)).sortBy(_._1)
        if (distances.count(_._1 == distances.head._1) == 1) map(y - minY)(x - minX) = ids(distances.head._2)
      })
    )
    map
  }

  private def findLargestArea(map: Array[Array[Int]]): Long = {
    val infiniteIDs = map.head.toSet ++ map.last.toSet ++ map.foldLeft(Set.empty[Int])((a, r) => a ++ Set(r.head, r.last))
    val confinedAreas = map.map(r => r.filterNot(infiniteIDs.contains))
    val grouped = confinedAreas.flatMap(_.toList).groupBy(d => d)
    grouped.map(_._2.length).max
  }

  private def findSafeArea(locations: List[Point], maxRange: Int): List[Point] = {
    val minX = locations.minBy(_._1)._1
    val maxX = locations.maxBy(_._1)._1
    val minY = locations.minBy(_._2)._2
    val maxY = locations.maxBy(_._2)._2
    (minY to maxY).flatMap(y =>
      (minX to maxX).map(x => (y, x)
    )).toList.filter(p => locations.map(distance(p, _)).sum < maxRange)
  }

  override def exampleAnswerPart1: Long = 17
  override def solvePart1(lines: List[String]): Long = {
    val locations = lines.map(toLocation)
    val map = mapAreas(locations)
    findLargestArea(map)
  }

  override def exampleAnswerPart2: Long = 16
  override def solvePart2(lines: List[String]): Long = {
    val maxRange = if (lines.length == 6) 32 else 10000
    val locations = lines.map(toLocation)
    findSafeArea(locations, maxRange).size
  }

}

object Day06 extends App {
  new Day06().solvePuzzles("/2018/day06.txt")
}
