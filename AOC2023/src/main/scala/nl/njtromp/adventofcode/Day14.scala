package nl.njtromp.adventofcode

class Day14 extends Puzzle[Long] with SimpleMapTypes {

  private def tiltNorth(map: SimpleMap[Char]): Unit =
    (1 until map.height).foreach(r =>
      (0 until map.width).foreach(c =>
        if map((r, c)) == 'O' then
          var rock = r
          while rock >= 1 && map((rock - 1, c)) == '.' do
            map((rock - 1, c)) = 'O'
            map((rock, c)) = '.'
            rock -= 1
      )
    )

  private def tiltSouth(map: SimpleMap[Char]): Unit =
    var r = map.height - 2
    while r >= 0 do
      (0 until map.width).foreach(c =>
        if map((r, c)) == 'O' then
          var rock = r
          while rock < map.height - 1 && map((rock + 1, c)) == '.' do
            map((rock + 1, c)) = 'O'
            map((rock, c)) = '.'
            rock += 1
      )
      r -= 1

  private def tiltWest(map: SimpleMap[Char]): Unit =
    (1 until map.width).foreach(c =>
      (0 until map.height).foreach(r =>
        if map((r, c)) == 'O' then
          var rock = c
          while rock >= 1 && map((r, rock - 1)) == '.' do
            map((r, rock - 1)) = 'O'
            map((r, rock)) = '.'
            rock -= 1
      )
    )

  private def tiltEast(map: SimpleMap[Char]): Unit =
    var c = map.width - 2
    while c >= 0 do
      (0 until map.height).foreach(r =>
        if map((r, c)) == 'O' then
          var rock = c
          while rock < map.width - 1 && map((r, rock + 1)) == '.' do
            map((r, rock + 1)) = 'O'
            map((r, rock)) = '.'
            rock += 1
      )
      c -= 1

  private def determineLoad(map: SimpleMap[Char]) = {
    (0 until map.height).map(r =>
      val rocks = (0 until map.width).count(c => map((r, c)) == 'O')
      rocks * (map.height - r)
    ).sum
  }

  override def exampleAnswerPart1: Long = 136
  override def solvePart1(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    tiltNorth(map)
    determineLoad(map)

  override def exampleAnswerPart2: Long = 64
  override def solvePart2(lines: List[String]): Long =
    val map = SimpleMap(lines, _.toCharArray)
    // With some luck just 1_000 cycles works instead of the required 1_000_000_000 :-)
    var cycles = 1000L
    while cycles > 0 do
      tiltNorth(map)
      tiltWest(map)
      tiltSouth(map)
      tiltEast(map)
      cycles -= 1
    determineLoad(map)

}

object Day14 extends App {
  new Day14().solvePuzzles()
}
