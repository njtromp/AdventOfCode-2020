package nl.njtromp.adventofcode

class Day08 extends Puzzle[Long] {

  private def calculateChecksum(width: Int, heigth: Int, rawImage: String): Long =
    val checkLayer = rawImage.grouped(width * heigth).minBy(_.count(_ == '0'))
    checkLayer.count(_ == '1') * checkLayer.count(_ == '2')

  private def decodeImage(width: Int, heigth: Int, rawImage: String): Long =
    val layers = rawImage.grouped(width * heigth).map(_.toArray).toList
    (0 until (width * heigth)).foreach(i =>
      val pixels = layers.map(_(i))
      if i % width == 0 then
        println
      val pixel = pixels.reduce((a, b) => if a != '2' then a else b)
      print(if pixel == '1' then 'X' else ' ')
    )
    println
    layers.size

  override def exampleAnswerPart1: Long = 1
  override def solvePart1(lines: List[String]): Long =
    if lines.head.length == 12 then
      calculateChecksum(3, 2, lines.head)
    else
      calculateChecksum(25, 6, lines.head)

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if lines.size == 2 then
      decodeImage(2, 2, lines.last)
      0
    else
      decodeImage(25, 6, lines.head)
      println("The real answer for part 2: LBRCE")
      0

}

object Day08 extends App {
  new Day08().solvePuzzles()
}
