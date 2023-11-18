package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day20 extends Puzzle[Long] {
  private class Node(val value: Long) {
    var next: Node = _
    var previous: Node = _
    private def remove(): Node =
      next.previous = previous
      previous.next = next
      next = null
      previous = null
      this
    private def insert(other: Node): Unit =
      other.next = next
      other.previous = this
      next.previous = other
      next = other
    @tailrec
    final def findTarget(n: Long): Node =
      if (n > 0)
        next.findTarget(n - 1)
      else if (n < 0)
        previous.findTarget(n + 1)
      else
        this
    def move(): Unit =
      if (value % Node.length != 0) {
        val target = findTarget((if (value < 0) (value - 1) % (Node.length - 1) else (value) % (Node.length - 1)))
        if (target.next != this)
          target.insert(remove())
        else
          println("!")
      }
  }

  override def exampleAnswerPart1: Long = 3
  override def solvePart1(lines: List[String]): Long = {
    val encrypted = lines.map(a => new Node(a.toLong))
    Node.length = encrypted.size
    encrypted.zip(encrypted.tail).foreach(a =>
      a._1.next = a._2
      a._2.previous = a._1
    )
    encrypted.head.previous = encrypted.last
    encrypted.last.next = encrypted.head
    encrypted.foreach(n => n.move())
    val zero = encrypted.find(_.value == 0).get
    zero.findTarget(1000).value +
    zero.findTarget(2000).value +
    zero.findTarget(3000).value
  }

  override def exampleAnswerPart2: Long = 1623178306L
  override def solvePart2(lines: List[String]): Long = {
    val encrypted = lines.map(a => new Node(a.toLong * 811589153L))
    Node.length = encrypted.size
    encrypted.zip(encrypted.tail).foreach(a =>
      a._1.next = a._2
      a._2.previous = a._1
    )
    encrypted.head.previous = encrypted.last
    encrypted.last.next = encrypted.head
    (1 to 10).foreach(_ => encrypted.foreach(n => n.move()))
    val zero = encrypted.find(_.value == 0).get
    zero.findTarget(1000).value +
      zero.findTarget(2000).value +
      zero.findTarget(3000).value
  }

  private def printFile(first: Node): Unit =
    var current = first
    while
      print(s"${current.value}, ")
      current = current.next
      current != first
    do ()
    println

  object Node {
    var length: Int = 0
  }

}

object Day20 extends App{
  new Day20().solvePuzzles("/day20.txt")
}
