package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day11 extends Puzzle[Long] with SimpleMapTypes {

  @tailrec
  private def expandRows(starsOnRow: List[(Long, Long)], emptyRows: List[Long], extraRows: Long): List[(Long, Long)] =
    if emptyRows.isEmpty then
      starsOnRow
    else
      val beforeRow = starsOnRow.filter(_._1 < emptyRows.head)
      val afterRow = starsOnRow.filter(_._1 > emptyRows.head)
      expandRows(beforeRow ++ afterRow.map(s => (s._1 + extraRows, s._2)), emptyRows.tail.map(_ + extraRows), extraRows)

  @tailrec
  private def expandColumns(starsOnColumn: List[(Long, Long)], emptyColumns: List[Long], extraColumns: Long): List[(Long, Long)] =
    if emptyColumns.isEmpty then
      starsOnColumn
    else
      val beforeColumn = starsOnColumn.filter(_._2 < emptyColumns.head)
      val afterColumn = starsOnColumn.filter(_._2 > emptyColumns.head)
      expandColumns(beforeColumn ++ afterColumn.map(s => (s._1, s._2 + extraColumns)), emptyColumns.tail.map(_ + extraColumns), extraColumns)

  private def distance(a: (Long, Long), b: (Long, Long)): Long =
    Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  private def expandGalaxy(lines: List[String], expension: Long) = {
    val stars = lines.zipWithIndex.flatMap(r => r._1.toCharArray.zipWithIndex.filter(_._1 == '#').map(s => (r._2.toLong, s._2.toLong)))
    val starsOnRow = stars.groupBy(_._1)
    val minRow = stars.map(_._1).min
    val maxRow = stars.map(_._1).max
    val emptyRows = ((minRow to maxRow).toSet diff starsOnRow.keySet).toList.sorted
    val rowExpandStars = expandRows(stars, emptyRows, expension - 1)

    val starsOnColumn = rowExpandStars.groupBy(_._2)
    val minColumn = rowExpandStars.map(_._2).min
    val maxColumn = rowExpandStars.map(_._2).max
    val emptyColumns = ((minColumn to maxColumn).toSet diff starsOnColumn.keySet).toList.sorted
    val expandedGalaxy = expandColumns(rowExpandStars, emptyColumns, expension - 1)
    expandedGalaxy.map(a => expandedGalaxy.map(b => distance(a, b)).sum).sum / 2
  }

  override def exampleAnswerPart1: Long = 374
  override def solvePart1(lines: List[String]): Long =
    expandGalaxy(lines, 2)

  override def exampleAnswerPart2: Long = 82000210
  override def solvePart2(lines: List[String]): Long =
    expandGalaxy(lines, 1000000)

}

object Day11 extends App {
  new Day11().solvePuzzles()
}
