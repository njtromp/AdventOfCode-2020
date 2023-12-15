package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.annotation.tailrec

class Day15 extends Puzzle[Long] {
  private val instruction = "(\\w+)(=|-)(\\d+)?".r

  private def hash(text: String): Long =
    @tailrec
    def hash(text: String, value: Long): Long =
      if text.isEmpty then
        value
      else
        hash(text.tail, ((value + text.head.toInt) * 17) % 256)
    hash(text, 0L)

  override def exampleAnswerPart1: Long = 1320
  override def solvePart1(lines: List[String]): Long =
    lines.head.split(",").map(hash).sum

  override def exampleAnswerPart2: Long = 145
  override def solvePart2(lines: List[String]): Long =
    val boxes = mutable.Map.empty[Long, List[(String, Int)]].withDefaultValue(List.empty)
    lines.head.split(',').foreach({case instruction(label, op, lens) =>
      val boxNr = hash(label)
      val box = boxes(boxNr)
      if op == "=" then
        val lensNr = lens.toInt
        if box.exists(_._1 == label) then
          boxes += boxNr -> box.map(l => if l._1 == label then (label, lensNr) else l)
        else
          boxes += boxNr -> (box ++ List((label, lensNr)))
      else if op == "-" then
        boxes += boxNr -> box.filter(_._1 != label)
    })
    boxes.flatMap((k, ls) => ls.zipWithIndex.map(l => (k + 1) * (l._2 + 1) * l._1._2)).sum

}

object Day15 extends App {
  new Day15().solvePuzzles()
}
