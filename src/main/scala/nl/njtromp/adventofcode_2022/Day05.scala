package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day05 extends Puzzle2 {
  override def exampleAnswerPart1: Long = 0

  def createPiles(crates: List[String]): ArrayBuffer[List[Char]] = {
    @tailrec
    def parseCrates(piles: ArrayBuffer[List[Char]], crates: List[String]): ArrayBuffer[List[Char]] = {
      crates match {
        case Nil => piles
        case c :: remainingCrates =>
          piles.indices.foreach(i => {
            val index = 1 + 4 * i
            if (index < c.length && c.charAt(index) != ' ') {
              piles(i) = c.charAt(index) :: piles(i)
            }
          })
          parseCrates(piles, remainingCrates)
      }
    }

    val piles = new mutable.ArrayBuffer[List[Char]]()
    (0 until crates.head.replace(" ", "").length).foreach(_ => piles += Nil)
    parseCrates(piles, crates.tail)
  }

  override def solvePart1(lines: List[String]): Long = {
    @tailrec
    def moveCretes(piles: ArrayBuffer[List[Char]], instructions: List[String]): ArrayBuffer[List[Char]] = {
      @tailrec
      def moveCreaes(piles: ArrayBuffer[List[Char]], crates: Int, from: Int, to: Int): ArrayBuffer[List[Char]] = {
        crates match {
          case 0 => piles
          case _ =>
            piles(to) = piles(from).head :: piles(to)
            piles(from) = piles(from).tail
            moveCreaes(piles, crates - 1, from, to)
        }
      }
    instructions match {
      case Nil => piles
      case instruction :: remainingInstructions =>
        val parts = instruction.split(" ")
        moveCretes(moveCreaes(piles, parts(1).toInt, parts(3).toInt - 1, parts(5).toInt - 1), remainingInstructions)
    }
  }

    val crates = lines.takeWhile(_.nonEmpty).reverse
    val instructions = lines.drop(crates.size + 1)
    val piles = moveCretes(createPiles(crates), instructions)
    piles.map(_.head).foreach(print)
    println
    0
  }

  override def exampleAnswerPart2: Long = 0

  override def solvePart2(lines: List[String]): Long = {
    @tailrec
    def moveCretes(piles: ArrayBuffer[List[Char]], instructions: List[String]): ArrayBuffer[List[Char]] = {
      def moveCreaes(piles: ArrayBuffer[List[Char]], crates: Int, from: Int, to: Int): ArrayBuffer[List[Char]] = {
        piles(to) = piles(from).take(crates) ++ piles(to)
        piles(from) = piles(from).drop(crates)
        piles
      }

      instructions match {
        case Nil => piles
        case instruction :: remainingInstructions =>
          val parts = instruction.split(" ")
          moveCretes(moveCreaes(piles, parts(1).toInt, parts(3).toInt - 1, parts(5).toInt - 1), remainingInstructions)
      }
    }

    val crates = lines.takeWhile(_.nonEmpty).reverse
    val instructions = lines.drop(crates.size + 1)
    val piles = moveCretes(createPiles(crates), instructions)
    piles.map(_.head).foreach(print)
    println
    0
  }
}

object Day05 extends App {
  new Day05().solvePuzzles("/2022/day05.txt")
}
