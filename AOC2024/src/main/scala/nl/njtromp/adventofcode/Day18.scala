package nl.njtromp.adventofcode

class Day18 extends Puzzle[String] with RouteFinding {

  private def parseByteLocation(line: String): Pos =
    line match
      case s"$x,$y" => (y.toInt, x.toInt)

  private def neighbours(bytes: List[Pos], size: Int)(p: Pos): List[Pos] =
    SQUARE.map(p + _).filter(p => p._1 >= 0 && p._1 <= size && p._2 >= 0 && p._2 <= size).filterNot(bytes.contains)

  override def exampleAnswerPart1: String = "22"
  override def solvePart1(lines: List[String]): String =
    val fallingBytes = lines.map(parseByteLocation)
    val finish: Pos = if fallingBytes.length < 1024 then (6, 6) else (70, 70)
    val bytes = if fallingBytes.length < 1024 then 12 else 1024
    val path = dijkstra((0, 0), finish, neighbours(fallingBytes.take(bytes), finish._1), (_, _) => 1)
    s"${path.size - 1}"

  override def exampleAnswerPart2: String = "6,1"
  override def solvePart2(lines: List[String]): String =
    val fallingBytes = lines.map(parseByteLocation)
    val finish: Pos = if fallingBytes.length < 1024 then (6, 6) else (70, 70)
    var low: Int = if fallingBytes.length < 1024 then 12 else 1024
    var high: Int = fallingBytes.length
    while low != high - 1 do
      val bytes = (low + high) / 2
      if dijkstra((0, 0), finish, neighbours(fallingBytes.take(bytes), finish._1), (_, _) => 1).isEmpty then
        high = bytes
      else
        low = bytes
    val blocking = fallingBytes(low)
    s"${blocking._2},${blocking._1}"

}

object Day18 extends App {
  new Day18().solvePuzzles()
}
