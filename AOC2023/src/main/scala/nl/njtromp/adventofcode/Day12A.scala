package nl.njtromp.adventofcode

// A slow approach
class Day12A extends Puzzle[Long] {
  private case class Record(springs: String, demageCounts: List[Int])

  private def parseLine(line: String): Record =
    val parts = line.split(' ')
    Record(parts.head, parts.last.split(',').map(_.toInt).toList)

  private def permute(springs: String): List[String] =
    val unknown = springs.indexOf('?')
    if unknown == -1 then
      List(springs)
    else
      val prefix = springs.substring(0, unknown)
      val remainder = springs.substring(unknown + 1)
      permute(remainder).flatMap(r => List(prefix + '.' + r, prefix + '#' + r))

  private def countDamagedGroupLengths(springs: String): List[Int] =
    springs.split('.').map(_.count(_ == '#')).filter(_ != 0).toList

  private def countPermutations(record: Record): Long =
    val bla = permute(record.springs).map(countDamagedGroupLengths).count(_ == record.demageCounts)
//    println(bla)
    bla

  private def unfold(record: Record): Record =
    val unfoldCount = 5
    Record((record.springs + '?').repeat(unfoldCount).dropRight(1), (1 to unfoldCount).toList.flatMap(_ => record.demageCounts))

  override def exampleAnswerPart1: Long = 21
  override def solvePart1(lines: List[String]): Long = {
    val perms = lines.map(parseLine).map(countPermutations)
    println(perms)
    perms.sum
  }

  override def exampleAnswerPart2: Long = 525152
  override def solvePart2(lines: List[String]): Long =
    println("Part 2")
    lines.map(l => unfold(parseLine(l))).map(countPermutations).sum

}

object Day12A extends App {
  new Day12A().solvePuzzles("/day12.txt")
}
