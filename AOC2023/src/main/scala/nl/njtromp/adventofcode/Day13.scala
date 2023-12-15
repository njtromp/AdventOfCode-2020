package nl.njtromp.adventofcode

class Day13 extends Puzzle[Long] {

  private def mirrorLine(lines: List[String]): Set[Long] =
    val mirroredLines = lines.zipWithIndex.groupBy(_._1).filter(_._2.length > 1).values.map(_.map(_._2))
    if mirroredLines.nonEmpty then
      val uniqueLines =  mirroredLines.flatten.toSet.map(_.toLong)
      uniqueLines.filter(l => lines.take(l.toInt).reverse.zip(lines.drop(l.toInt)).forall(_ == _))
    else
      Set.empty

  private def mirrorColumn(lines: List[String]): Set[Long] =
    val mappedLines = SimpleMap(lines, _.toCharArray)
    mirrorLine(mappedLines.columns())

  private def findMirror(lines: List[String]): Long =
    val column = mirrorColumn(lines)
    val row = mirrorLine(lines)
    column.sum + 100 * row.sum

  private def fixSmudge(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    val oldColumn = mirrorLine(map.columns())
    val oldRow = mirrorLine(map.rows())

    val mirrors = (0 until map.height).flatMap(r => (0 until map.width).map(c => (r, c))).map(p =>
      map(p) = if map(p) == '.' then '#' else '.'
      val column = (mirrorLine(map.columns()) -- oldColumn).filter(_ > 0)
      val row = (mirrorLine(map.rows()) -- oldRow).filter(_ > 0)
      val score = (if column.isEmpty then 0 else column.head) + 100 * (if row.isEmpty then 0 else row.head)
      map(p) = if map(p) == '.' then '#' else '.'
      score
    ).filter(_ > 0).toSet

    if mirrors.size == 1 then
      mirrors.head
    else
      0

  override def exampleAnswerPart1: Long = 405
  override def solvePart1(lines: List[String]): Long =
    groupByEmptyLine(lines).map(findMirror).sum

  override def exampleAnswerPart2: Long = 400
  override def solvePart2(lines: List[String]): Long =
    groupByEmptyLine(lines).map(fixSmudge).sum

}

object Day13 extends App {
  new Day13().solvePuzzles()
}
