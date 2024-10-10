package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec
import scala.io.Source

class Day23 extends Puzzle {

  private type Amphipod = (Char, Int, Int)
  private type Pos = (Int, Int)
  private val AMPHIPODS = "ABCD"

  private val finalMaze: List[String] = "#############\n#...........#\n###A#B#C#D###\n  #A#B#C#D#\n  #########".split("\n").toList
  private val targetColumn = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)

  case class Maze(maze: List[String]) {
    def print(): Unit = {
      maze.foreach(l => println(l.mkString))
      println
    }
    def costsPerMove(amphipod: Char): Int = Math.pow(10, amphipod - 'A').toInt
    def filterAmphipods: List[Amphipod] =
      maze.zipWithIndex.flatMap(l => l._1.zipWithIndex.map(a => (a._1, a._2, l._2))).filter(a => AMPHIPODS.contains(a._1))
    def movingCosts(a: Amphipod, target: Pos): Int = {
      if (a._2 == target._1)
        costsPerMove(a._1) * Math.abs(a._3 - target._2)
      else
        costsPerMove(a._1) * (Math.abs(a._2 - target._1) + (a._3 - 1) + (target._2 - 1))
    }
    def needsToMove(): List[Amphipod] = filterAmphipods.filterNot(isOnFinalLocation)
    def isOnFinalLocation(a: Amphipod): Boolean =
      a._2 == targetColumn(a._1) && (a._3 == 3 || (a._3 == 2 && maze(3)(a._2) == a._1))
    def horizontalMoves(x: Int): List[Int] = {
      ((x - 1 until 0 by -1).takeWhile(maze(1)(_) == '.').toList ++
        (x + 1 until 13).takeWhile(maze(1)(_) == '.').toList)
    }
    def intoTarget(a: Amphipod, x: Option[Int]): List[Pos] = {
      x match {
        case None => List.empty
        case Some(x) => if (maze(3)(x) == '.' && maze(2)(x) == '.')
          List((x, 3))
        else if (maze(2)(x) == '.' && maze(3)(x) == a._1)
          List((x, 2))
        else
          List.empty
      }
    }
    def findPossibleLocations(a: Amphipod): List[Pos] = {
      if (a._3 == 1) {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else if (a._3 == 2) {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else if (maze(2)(a._2) == '.') {
          val hTargets = horizontalMoves(a._2)
          hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else
        List.empty
    }
    def moveAmphipod(a: Amphipod, target: Pos): Maze = {
      Maze(updateMaze(a, target))
    }
    def updateMaze(a: Amphipod, target: Pos): List[String] = {
      val newMaze = maze.toArray.map(_.toArray)
      newMaze(a._3)(a._2) = '.'
      newMaze(target._2)(target._1) = a._1
      newMaze.map(_.mkString).toList
    }
  }

  class Maze2(maze2: List[String]) extends Maze(maze2) {
    override def intoTarget(a: Amphipod, x: Option[Int]): List[Pos] = {
      x match {
        case None => List.empty
        case Some(x) => if (maze(2)(x) == '.' && maze(3)(x) == '.' && maze(4)(x) == '.' && maze(5)(x) == '.')
          List((x, 5))
        else if (maze(2)(x) == '.' && maze(3)(x) == '.' && maze(4)(x) == '.' && maze(5)(x) == a._1)
          List((x, 4))
        else if (maze(3)(x) == '.' && maze(3)(x) == '.' && maze(4)(x) == a._1 && maze(5)(x) == a._1)
          List((x, 3))
        else if (maze(2)(x) == '.' && maze(3)(x) == a._1 && maze(4)(x) == a._1 && maze(5)(x) == a._1)
          List((x, 2))
        else
          List.empty
      }
    }
    override def findPossibleLocations(a: Amphipod): List[Pos] = {
      if (a._3 == 1) {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else if (a._3 == 2) {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else if (maze(2)(a._2) == '.') {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else if (maze(2)(a._2) == '.' && maze(3)(a._2) == '.') {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else if (maze(2)(a._2) == '.' && maze(3)(a._2) == '.' && maze(4)(a._2) == '.') {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(a, hTargets.find(_ == targetColumn(a._1)))
      } else
        List.empty
    }
    override def moveAmphipod(a: Amphipod, target: Pos): Maze = {
      new Maze2(updateMaze(a, target))
    }
  }

  def solveMaze(maze: Maze): Long = {
    var bestCosts = Long.MaxValue
    def solveMaze(costs: Long, current: Maze, visited: Set[Maze]): Unit = {
//      current.print()
      if (current.maze == finalMaze) {
        if (costs < bestCosts)
          bestCosts = costs
      } else {
        val moves: List[(Amphipod, Pos)] = current.needsToMove().flatMap(a => current.findPossibleLocations(a).map((a, _)))
        moves.foreach(m => {
          val newMaze = current.moveAmphipod(m._1, m._2)
          if (!visited.contains(newMaze)) solveMaze(costs + current.movingCosts(m._1, m._2), newMaze, visited + newMaze)
        })
      }
    }
    solveMaze(0, maze, Set.empty)
    bestCosts
  }

  override def solvePart1(lines: List[String]): Long = {
    // 17400
    solveMaze(Maze(lines))
  }

  override def solvePart2(lines: List[String]): Long = {
    // 46162 is too high
    // 46140 ???
    // 46127 ???
    // 46092 is too low
    solveMaze(new Maze2(lines))
  }
}

object Day23 extends App {
  val lines1: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day23-1.txt")).getLines().toList
  val lines2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day23-2.txt")).getLines().toList

  println(s"Answer ${getClass.getSimpleName} part 2: ${new Day23().solvePart2(lines2)}")
  println(s"Answer ${getClass.getSimpleName} part 1: ${new Day23().solvePart1(lines1)}")
}

