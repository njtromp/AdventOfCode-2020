package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode_2020.Puzzle

import scala.annotation.tailrec

class Day03 extends Puzzle {
  def decodeBit(c: Char): Int = if (c == '1') 1 else -1

  override def solvePart1(lines: List[String]): Long = {
    val decoded = lines.map(_.toCharArray.map(decodeBit))
    val combined = decoded.reduce((a, b) => a.zip(b).map((x) => x._1 + x._2))
    val gammaRate = combined.foldLeft(0)((v, b) => v * 2 + (if (b > 0) 1 else 0))
    val epsilonRate = combined.foldLeft(0)((v, b) => v * 2 + (if (b < 0) 1 else 0))
    gammaRate * epsilonRate
  }

  override def solvePart2(lines: List[String]): Long = {
    val decoded = lines.map(_.toCharArray.map(decodeBit))
    @tailrec
    def o2Rating(v: Int, decoded: List[Array[Int]]): Long = {
      if (decoded.size == 1) {
        if (decoded.head.length == 0) {
          v
        } else {
          o2Rating(v * 2 + (if (decoded.head.head == 1) 1 else 0), List(decoded.head.tail))
        }
      } else {
        val bit = if (decoded.foldLeft(0)((v, b) => v + b.head) >= 0) 1 else -1;
        o2Rating(v * 2 + (if (bit == 1) 1 else 0), decoded.filter(_.head == bit).map(_.tail))
      }
    }
    @tailrec
    def co2Rating(v: Int, decoded: List[Array[Int]]): Long = {
      if (decoded.size == 1) {
        if (decoded.head.length == 0) {
          v
        } else {
          co2Rating(v * 2 + (if (decoded.head.head == 1) 1 else 0), List(decoded.head.tail))
        }
      } else {
        val bit = if (decoded.foldLeft(0)((v, b) => v + b.head) >= 0) -1 else 1;
        co2Rating(v * 2 + (if (bit == 1) 1 else 0), decoded.filter(_.head == bit).map(_.tail))
      }
    }
    o2Rating(0, decoded) * co2Rating(0, decoded)
  }
}

object Day03 extends App {
  new Day03().solvePuzzles("/2021/day03.txt")
}

