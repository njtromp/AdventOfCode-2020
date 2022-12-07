package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.collection.mutable.ArrayBuffer

abstract class Node {
  def totalSize: Long
}

class ElvenFile(val name: String, val size: Int) extends Node {
  override def toString: String = s"$name: $size"
  override def totalSize: Long = size
}

class ElvenFolder(val name: String, var parent: ElvenFolder, val children: ArrayBuffer[Node]) extends Node {
  def selectFolder(name: String): ElvenFolder = {
    children.find({
      case f: ElvenFolder => f.name == name
      case _ => false
    }).get match {
      case f: ElvenFolder =>f
    }
  }
  def root: ElvenFolder = if (parent == this) this else parent.root
  override def totalSize: Long = children.map(_.totalSize).sum
  override def toString: String = s"$name $children"
}

class Day07 extends Puzzle2 {

  override def exampleAnswerPart1: Long = 95437

  def parseFolders(lines: List[String], folder: ElvenFolder): List[String] = {
    def createFolder(file: String): Node = {
      new ElvenFolder(file.split(" ")(1), folder, ArrayBuffer())
    }
    def createFile(file: String): Node = {
      new ElvenFile(file.split(" ")(1), file.split(" ")(0).toInt)
    }
    if (lines.isEmpty) {
      lines
    } else if (lines.head == "$ cd /") {
      parseFolders(lines.tail, folder.root)
    } else if (lines.head == "$ ls") {
      val content = lines.tail.takeWhile(l => l.startsWith("dir") || l.split(" ")(0)(0).isDigit)
      content.foreach(c =>
        if (c.startsWith("dir")) {
          folder.children += createFolder(c)
        } else {
          folder.children += createFile(c)
        }
      )
      parseFolders(lines.tail.drop(content.size), folder)
    } else if (lines.head == "$ cd ..") {
      parseFolders(lines.tail, folder.parent)
    } else if (lines.head.startsWith("$ cd")) {
      parseFolders(lines.tail, folder.selectFolder(lines.head.split(" ")(2)))
    } else if (lines.head == "$ ls") {
      parseFolders(lines.tail, folder)
    } else {
      List()
    }
  }

  def listFolderSizes(folder: ElvenFolder): List[Long] = {
    folder.totalSize :: folder.children.flatMap({
      case f: ElvenFolder => listFolderSizes(f)
      case _ => List()
    }).toList
  }

  override def solvePart1(lines: List[String]): Long = {
    val root = new ElvenFolder("/", null, ArrayBuffer())
    root.parent = root;
    parseFolders(lines, root)
    listFolderSizes(root).filter(_ <= 100000).sum
  }

  override def exampleAnswerPart2: Long = 24933642

  override def solvePart2(lines: List[String]): Long = {
    val root = new ElvenFolder("/", null, ArrayBuffer())
    root.parent = root;
    parseFolders(lines, root)
    val maxSpace = 70000000L
    val neededFreeSpace = 30000000L
    val minimumFolderSize = neededFreeSpace - (maxSpace - root.totalSize)
    listFolderSizes(root).filter(_ >= minimumFolderSize).min
  }

}

object Day07 extends App {
  new Day07().solvePuzzles("/2022/day07.txt")
}
