package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day09 extends Puzzle[Long] {

  private def parse(line: String): (List[Int], List[Int]) =
    val lists = line.toCharArray.map(_.asDigit).toList
    val files = lists.sliding(1, 2).map(_.head).toList
    val gaps = lists.tail.sliding(1, 2).map(_.head).toList
    (files, gaps)

  private def defragFileSystem(blocks: (List[Int], List[Int])): Long =
    val files = blocks._1
    val gaps = blocks._2
    val fileIds = files.zipWithIndex.flatMap((l, f) => (1 to l).map(_ => f))
    val gapIds = files.zipWithIndex.reverse.flatMap((l, f) => (1 to l).map(_ => f))
    val blockCount = files.sum
    @tailrec
    def check(index: Long, checksum: Long, files: List[Int], fileIds: List[Int], gaps: List[Int], gapIds: List[Int]): Long =
      if index < blockCount then
        val fileSize = Math.min(files.head, blockCount - index).toInt
        val fileChecksum = (index until index + fileSize).zip(fileIds).map(_ * _).sum
        val gapSize = Math.min(gaps.head, blockCount - index - fileSize).toInt
        val gapChecksum = (index + fileSize until index + fileSize + gapSize).zip(gapIds).map(_ * _).sum
        check(
          index + fileSize + gapSize,
          checksum + fileChecksum + gapChecksum,
          files.tail,
          fileIds.drop(fileSize),
          gaps.tail,
          gapIds.drop(gapSize)
        )
      else
        checksum
    check(0, 0L, files, fileIds, gaps, gapIds)

  private def defragFiles(blocks: (List[Int], List[Int])): Long =
    def headOrZero(gaps: List[Int]): Int =
      if gaps.isEmpty then 0 else gaps.head
    @tailrec
    def defrag(moveIndex: Int, files: List[(Int, Int)], gaps: List[Int]): (List[(Int, Int)], List[Int]) =
      if moveIndex == 0 then
        (files, gaps)
      else
        val file = files(moveIndex)
        val gapIndex = gaps.zipWithIndex.find(_._1 >= file._1).getOrElse((0, moveIndex))._2
        if gapIndex < moveIndex then
          val (filesHead, filesRemainder) = files.splitAt(gapIndex + 1)
          val (gapsHead, gapsRemainder) = gaps.splitAt(gapIndex)
          val (filesMid, filesTail) = filesRemainder.splitAt(moveIndex - gapIndex - 1)
          val (gapsMid, gapsTail) = gapsRemainder.splitAt(moveIndex - gapIndex - 1)
          val fileToMove = filesTail.take(1).head
          val (newFiles, newGaps) = if gapsMid.isEmpty then
            (filesHead ++ List(fileToMove) ++ filesTail.tail, gapsHead ++ List(0) ++ List(fileToMove._1 + headOrZero(gapsTail.drop(1))) ++ gapsTail.drop(2))
          else
            (filesHead ++ List(fileToMove) ++ filesMid ++ filesTail.tail, gapsHead ++ List(0, headOrZero(gapsMid) - fileToMove._1) ++ gapsMid.drop(1) ++ List(gapsTail.head + fileToMove._1 + headOrZero(gapsTail.drop(1))) ++ gapsTail.drop(2))
          defrag(moveIndex, newFiles, newGaps)
        else
          defrag(moveIndex - 1, files, gaps)
    @tailrec
    def check(index: Int, checksum: Long, files: List[(Int, Int)], gaps: List[Int]): Long =
      if files.isEmpty then
        checksum
      else
        val (fileSize, fileId) = files.head
        val fileChecksum = (index until index + fileSize).map(_ * fileId.toLong).sum
        val gapSize = gaps.head
        check(index + fileSize + gapSize, checksum + fileChecksum, files.tail, gaps.tail)
    val (files, gaps) = defrag(blocks._1.size - 1, blocks._1.zipWithIndex, blocks._2)
    check(0, 0, files, gaps)

  override def exampleAnswerPart1: Long = 1928
  override def solvePart1(lines: List[String]): Long =
    lines.map(l => defragFileSystem(parse(l))).sum

  override def exampleAnswerPart2: Long = 2858
  override def solvePart2(lines: List[String]): Long =
    lines.map(l => defragFiles(parse(l))).sum

}

object Day09 extends App {
  new Day09().solvePuzzles()
}
