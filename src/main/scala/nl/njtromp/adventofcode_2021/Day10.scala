package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day10 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    def score(line: String): Long = {
      val values = Map(')' -> 3L, ']' -> 57L, '}' -> 1197L, '>' -> 25137L)
      @tailrec
      def score(line: String, stack: List[Char]): Long ={
        if (line.isEmpty)
          0L
        else if ("([{<".contains(line.head))
            score(line.tail, line.head :: stack)
        else {
          if (line.head == ')' && stack.head == '(')
            score(line.tail, stack.tail)
          else if (line.head == ']' && stack.head == '[')
            score(line.tail, stack.tail)
          else if (line.head == '}' && stack.head == '{')
            score(line.tail, stack.tail)
          else if (line.head == '>' && stack.head == '<')
            score(line.tail, stack.tail)
          else
            values(line.head)
        }
      }
      score(line, List.empty[Char])
    }
    lines.map(score).sum
  }

  override def solvePart2(lines: List[String]): Long = {
    def score(line: String): Long = {
      val values = Map('(' -> 1L, '[' -> 2L, '{' -> 3L, '<' -> 4L)
      @tailrec
      def score(line: String, stack: List[Char]): Long ={
        if (line.isEmpty)
          stack.foldLeft(0L)((a, c) => a * 5L + values(c))
        else if ("([{<".contains(line.head))
          score(line.tail, line.head :: stack)
        else {
          if (line.head == ')' && stack.head == '(')
            score(line.tail, stack.tail)
          else if (line.head == ']' && stack.head == '[')
            score(line.tail, stack.tail)
          else if (line.head == '}' && stack.head == '{')
            score(line.tail, stack.tail)
          else if (line.head == '>' && stack.head == '<')
            score(line.tail, stack.tail)
          else
            0L
        }
      }
      score(line, List.empty[Char])
    }
    val scores = lines.map(score).filter(_ > 0).sorted
    scores(scores.length / 2)
  }
}

object Day10 extends App {
  new Day10().solvePuzzles("/2021/day10.txt")
}
