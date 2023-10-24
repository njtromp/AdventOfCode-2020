package nl.njtromp.adventofcode

import scala.collection.mutable

class Day07 extends Puzzle[Long] {

  private abstract class Node {
    def getName: String
    def isDir: Boolean
    def getSize: Long
  }

  private case class File(name: String, size: Long) extends Node {
    def getName: String = name
    def isDir: Boolean = false
    def getSize: Long = size
  }

  private case class Directory(name: String) extends Node {
    def getName: String = name
    def isDir: Boolean = true
    def getSize: Long = content.map(_.getSize).sum
    val content: mutable.ListBuffer[Node] = mutable.ListBuffer.empty[Node]
    def getDirectories: List[Directory] =
      val dirs = content.filter(_.isDir).map({case d: Directory => d}).toList
      dirs ++ dirs.flatMap(_.getDirectories)
  }

  private val cdDown = "\\$ cd (.+)".r
  private val cdUp = "\\$ cd \\.\\.".r
  private val ls = "\\$ ls".r
  private val file = "(\\d+) (.+)".r
  private val dir = "dir (.+)".r

  private val history = mutable.Stack.empty[Directory]

  @scala.annotation.tailrec
  private def processOutput(lines: List[String]): Directory = {
    if (lines.isEmpty)
      history.last
    else
      lines.head match {
        case cdUp() =>
          if (history.size > 1) history.pop
        case cdDown(dir) =>
          if (dir == "/")
            while (history.size > 1)
              history.pop()
          else
            val current = history.head
            current.content.filter(_.isDir).filter(_.getName == dir).head match {
              case target: Directory => history.push(target)
            }
        case file(size, name) =>
          history.head.content += File(name, size.toLong)
        case dir(name) =>
          history.head.content += Directory(name)
        case ls() =>
      }
      processOutput(lines.tail)
  }

  override def exampleAnswerPart1: Long = 95437L
  override def solvePart1(lines: List[String]): Long = {
    history.clear()
    history.push(Directory("/"))
    val root = processOutput(lines)
    (root :: root.getDirectories).filter(_.getSize <= 100000L).map(_.getSize).sum
  }

  override def exampleAnswerPart2: Long = 24933642L
  override def solvePart2(lines: List[String]): Long = {
    history.clear()
    history.push(Directory("/"))
    val root = processOutput(lines)
    val totalDiskSpace = 70000000L
    val minimumFreeDiskSpace = 30000000L
    val miniumDirectorySize = minimumFreeDiskSpace - (totalDiskSpace - root.getSize)
    (root :: root.getDirectories).filter(_.getSize >= miniumDirectorySize).minBy(_.getSize).getSize
  }

}

object Day07 extends App{
  new Day07().solvePuzzles("/day07.txt")
}
