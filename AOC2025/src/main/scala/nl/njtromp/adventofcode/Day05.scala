package nl.njtromp.adventofcode

class Day05 extends Puzzle[Long] {

  override def exampleAnswerPart1: Long = 3
  override def solvePart1(lines: List[String]): Long =
    val freshIds: List[LongRange] = lines.takeWhile(_.nonEmpty).map {
      case s"$first-$last" => LongRange(first.toLong, last.toLong)
    }
    val ingredients = lines.drop(freshIds.size + 1).map(_.toLong)
    ingredients.count(id => freshIds.exists(_.contains(id)))

  override def exampleAnswerPart2: Long = 14
  override def solvePart2(lines: List[String]): Long =
    def merge(ranges: List[LongRange]): List[LongRange] =
      if ranges.isEmpty then
        Nil
      else
        val range = ranges.head
        val overlapping = ranges.tail.filter(_.isOverlapping(range))
        if overlapping.isEmpty then
          range :: merge(ranges.tail)
        else {
          val combined = overlapping.flatMap(_.combine(range))
          merge(combined ++ ranges.tail.filterNot(overlapping.contains))
        }
    val freshIds: List[LongRange] = lines.takeWhile(_.nonEmpty).map {
      case s"$first-$last" => LongRange(first.toLong, last.toLong)
    }
    merge(freshIds).map(_.size).sum

}

object Day05 extends App {
  new Day05().solvePuzzles()
}
