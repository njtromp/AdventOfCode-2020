package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day24 extends Puzzle2 {
  type Pos = (Int, Int)
  case class ValleyState(progress: Int, expedition: Pos)
  case class Valley(height: Int, width: Int, left: Array[Array[Char]], right: Array[Array[Char]], up: Array[Array[Char]], down: Array[Array[Char]]) {
    private val repeating = height * width
    def apply(progress: Int, p: Pos): Char = {
      if (p._1 == -1 || p._1 == height)
        '.'
      else {
        val l = left(p._1)((p._2 + (progress % repeating)) % width)
        val r = right(p._1)((((p._2 - (progress % repeating)) % width) + width) % width)
        val u = up((p._1 + (progress % repeating)) % height)(p._2)
        val d = down((((p._1 - (progress % repeating)) % height) + height) % height)(p._2)
        val combined = s"$l$r$u$d".sorted
        if (combined.count(_ == '.') == 4)
          '.'
        else {
          val snowFlakes = combined.replaceAll("\\.", "")
          if (snowFlakes.length == 1)
            snowFlakes.charAt(0)
          else
            snowFlakes.length.toString.charAt(0)
        }
      }
    }
    def neighbours(state: ValleyState): List[ValleyState] = {
      val newProgress = (state.progress + 1) % repeating
      Set((0, 0), (0, -1), (0, 1), (-1, 0), (1, 0))
        .map(d => (state.expedition._1 + d._1, state.expedition._2 + d._2))
        .filter(e => {
          (e._1 >= 0 && e._1 < height && e._2 >= 0 && e._2 < width) || // Within the valley
          (e._1 == -1 && e._2 == 0) || // Start
          (e._1 == height && e._2 == width - 1)} // Finish
        )
        .filter(e => this(newProgress, e) == '.')
        .map(ValleyState(newProgress, _))
        .toList
    }
    def printValley(state: ValleyState): Unit = {
      if (state.expedition == (-1, 0))
        print("#E")
      else
        print("#.")
      println("#"*width)
      (0 until height).foreach(y => {
        print("#")
        (0 until width).foreach(x => {
          val p = (y, x)
          if (p == state.expedition)
            print("E")
          else
            print(this(state.progress, p))
        })
        println("#")
      })
      print("#"*width)
      println(".#")
    }
  }
  object Valley{
    def apply(lines: List[String]): Valley = {
      val left = lines.toArray.drop(1).dropRight(1).map(_.replaceAll("\\^|>|v", "\\.").toArray.drop(1).dropRight(1))
      val right = lines.toArray.drop(1).dropRight(1).map(_.replaceAll("\\^|<|v", "\\.").toArray.drop(1).dropRight(1))
      val up = lines.toArray.drop(1).dropRight(1).map(_.replaceAll("<|>|v", "\\.").toArray.drop(1).dropRight(1))
      val down = lines.toArray.drop(1).dropRight(1).map(_.replaceAll("\\^|<|>", "\\.").toArray.drop(1).dropRight(1))
      Valley(lines.length -2, lines.head.length - 2, left, right, up, down)
    }
  }

  private def findRoute(valley: Valley, progress: Int, start: Pos, finish: Pos): Int = {
    val distance = mutable.Map[ValleyState, Int]().withDefaultValue(Int.MaxValue)
    val source = mutable.Map[ValleyState, ValleyState]()
    val visited = mutable.Set[ValleyState]()
    val investigate = ArrayBuffer[ValleyState]()

    @tailrec
    def dijkstra(): Int = {
      if (investigate.isEmpty)
        -1
      else {
        val current = investigate.minBy(distance)
//        println("== PROCESSING ==")
//        valley.printValley(current)
        if (current.expedition == finish) return distance(current)
        investigate -= current
        visited += current
        val potentialDistance = distance(current) + 1
        valley.neighbours(current).foreach(n => {
          if (potentialDistance < distance(n)) {
            distance(n) = potentialDistance
//            println("-- CANDIDATE --")
//            valley.printValley(n)
            source += n -> current
          }
          if (!visited.contains(n) && !investigate.contains(n)) {
            investigate += n
          }
        })
        dijkstra()
      }
    }
    val startState = ValleyState(progress, start)
    distance(startState) = 0
    investigate += startState
    dijkstra()
  }

  override def exampleAnswerPart1: Long = 18
  override def solvePart1(lines: List[String]): Long = {
    findRoute(Valley(lines), 0, (-1, 0), (lines.length - 2, lines.head.length - 3))
  }

  override def exampleAnswerPart2: Long = 54
  override def solvePart2(lines: List[String]): Long = {
    val valley = Valley(lines)
    val entry = (-1, 0)
    val exit = (lines.length - 2, lines.head.length - 3)
    val toExit = findRoute(valley, 0, entry, exit)
    val toEntry = findRoute(valley, toExit, exit, entry)
    toExit + toEntry + findRoute(valley, toExit + toEntry, entry, exit)
  }

}

object Day24 extends App{
  new Day24().solvePuzzles("/2022/day24.txt")
}
