package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle

class Day03 extends Puzzle {
  type Pos = (Int, Int)
  type Size = (Int, Int)
  type Claim = (String, Pos, Size)

  private def createClaim(line: String): Claim = {
    val idParts = line.split("@").map(_.trim)
    val posSizeParts = idParts(1).split(":").map(_.trim)
    val posParts = posSizeParts(0).split(",").map(_.trim.toInt)
    val sizeParts = posSizeParts(1).split("x").map(_.trim.toInt)
    (idParts(0), (posParts(0), posParts(1)), (sizeParts(0), sizeParts(1)))
  }

  override def solvePart1(lines: List[String]): Long = {
    lines
      .map(createClaim)
      .flatMap(c => (c._2._1 until (c._2._1 + c._3._1)).flatMap(x => (c._2._2 until (c._2._2 + c._3._2)).map((x, _))))
      .groupBy(p => p)
      .count(_._2.length > 1)
  }

  private def layoutClaim(c: Claim): List[Pos] = {
    (c._2._1 until (c._2._1 + c._3._1)).flatMap(x => (c._2._2 until (c._2._2 + c._3._2)).map((x, _))).toList
  }

  override def solvePart2(lines: List[String]): Long = {
    val claims = lines.map(createClaim)
    val overlapping = claims
      .flatMap(layoutClaim)
      .groupBy(p => p)
      .filter(_._2.size > 1)
      .keys.toSet
    val id = claims.filter(c => layoutClaim(c).toSet.intersect(overlapping).isEmpty)
      .head._1
    println(id)
    -1
  }

}

object Day03 extends App {
  new Day03().solvePuzzles("/2018/day03.txt")
}
