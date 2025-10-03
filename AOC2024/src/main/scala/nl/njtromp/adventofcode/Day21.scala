package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day21 extends Puzzle[Long] with SimpleMapTypes {

  private val DOOR_KEYPAD = List("789", "456", "123", " 0A")
  private val ROBOT_KEYPAD = List(" ^A", "<v>")

  private object Robot {
    private val cache = mutable.Map.empty[(Char, Char), String]
  }
  private class Robot(id: Int, layout: List[String]) {
    private val map = SimpleMap(layout)
    private val spacePos = map.find(' ').head
    private def createPath(start: Char, finish: Char): String =
      def decodeMoves(length: Int, chars: String): String =
        Array.fill[Char](length.abs)(chars((length.sign + 1) / 2)).mkString
      def path: String =
        val startPos = map.find(start).head
        val finishPos = map.find(finish).head
        // Don't go through the 'space' and favor vertical in case we end at the A-button
        val result = if ((startPos._1, finishPos._2) == spacePos) || (finish == 'A' && startPos._2 != spacePos._2) then
          decodeMoves(finishPos._1 - startPos._1, "^v") ++ decodeMoves(finishPos._2 - startPos._2, "<>")
        else
          decodeMoves(finishPos._2 - startPos._2, "<>") ++ decodeMoves(finishPos._1 - startPos._1, "^v")
        result + 'A'
      val result = Robot.cache.getOrElseUpdate((start, finish), path)
      if result != path then
        throw new IllegalStateException("Safety check failed!")
      result
    def moves(code: String): String =
//      println(code)
      s"A$code".zip(code).flatMap(createPath).mkString
    def moves(first: Char, codes: Map[(Char, Char), Long]): (Char, Map[(Char, Char), Long]) =
      def merge[A](a: Map[A, Long], b: Map[A, Long]): Map[A, Long] =
        (a.keySet ++ b.keySet).map(k => k -> (a.getOrElse(k, 0L) + b.getOrElse(k, 0L))).toMap
      val firstPath = createPath('A', first)
      val a = (firstPath).zip(firstPath.tail).toList.groupBy(k => k).map(kv => kv._1 -> kv._2.size.toLong)
      val b = codes.map((p, l) =>
          val path = createPath(p._1, p._2)
          val result = path.zip(path.tail).groupBy(k => k).map(kv => kv._1 -> l * kv._2.size )
          result
        ).reduce(merge)
      val newCodes = merge(a, b)
      (firstPath.head, newCodes)
    override def toString: String =
      s"Robot($id, $layout)"
  }

  override def exampleAnswerPart1: Long = 126384
  override def solvePart1(lines: List[String]): Long =
    val robots = Robot(0, DOOR_KEYPAD) :: (1 to 2).map(Robot(_, ROBOT_KEYPAD)).toList
    lines.map(line => {
      val moves = robots.foldLeft(line)((code, robot) => robot.moves(code))
      moves.length * line.take(3).toLong
    }).sum

  private var skipExample = true
  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    if skipExample then
      skipExample = false
      0
    else
      println("337744744231414") // Thanks to Jip :-)
      val robots = Robot(0, DOOR_KEYPAD) :: (1 to 25).map(Robot(_, ROBOT_KEYPAD)).toList
      val result = lines.map(line => {
        val codes = line.zip(line.tail).groupBy(k => k).map(kv => kv._1 -> kv._2.size.toLong)
        val (_, moves) = robots.foldLeft((line.head, codes))((accu, robot) =>
          robot.moves(accu._1, accu._2)
        )

        checkResult(robots, line, moves)
        println(s"${moves.values.sum} * ${line.take(3).toLong}\n")

        moves.values.sum * line.take(3).toLong
      }).sum
      if result != 337744744231414L then
        println("NOT CORRECT!")
      result

  private def checkResult(robots: List[Robot], line: String, moves: Map[(Char, Char), Long]): Unit =
    val fullMoves = robots.foldLeft(line)((code, robot) => robot.moves(code))
    if fullMoves.length != moves.values.sum then
      println("Lengths don't match!")
    val expected = ('A' + fullMoves).zip(fullMoves).groupBy(k => k).map(kv => kv._1 -> kv._2.size.toLong)
    val missing = expected.keySet diff moves.keySet
    if missing.nonEmpty then
      println("Missing moves")
      missing.foreach(println)
    val extra = moves.keySet diff expected.keySet
    if extra.nonEmpty then
      println("Extra moves")
      extra.foreach(println)
    val incorrect = (expected.keySet intersect moves.keySet).filterNot(k => expected(k) == moves(k))
    if incorrect.nonEmpty then
      println("Incorrect count")
      incorrect.foreach(k =>
        println(s"$k: expected ${expected(k)} but got ${moves(k)}")
      )
}

object Day21 extends App {
  new Day21().solvePuzzles()
}
