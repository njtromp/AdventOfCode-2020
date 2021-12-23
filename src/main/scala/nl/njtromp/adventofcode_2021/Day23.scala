package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day23 extends Puzzle {

  private type Amphipod = (Char, Int, Int)
  private val AMPHIPODS = "ABCD"

  private val finalMaze: List[String] = "#############\n#...........#\n###A#B#C#D###\n  #A#B#C#D#\n  #########".split("\n").toList
  private val targetColumn = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)

  case class Maze(costs: Int, maze: List[String]) {
    def print(): Unit = {
      maze.foreach(l => println(l.mkString))
      println
    }
    def costsPerMove(amphipod: Char): Int = Math.pow(10, amphipod - 'A').toInt
    def expectedCosts: Int =
      filterAmphipods.groupBy(_._1).map(a => costsPerMove(a._1) * a._2.map(expectedMovesNeeded).sum).sum * 90 / 100
    def filterAmphipods: List[Amphipod] =
      maze.zipWithIndex.flatMap(l => l._1.zipWithIndex.map(a => (a._1, a._2, l._2))).filter(a => AMPHIPODS.contains(a._1))
    def expectedMovesNeeded(a: Amphipod): Int = {
      a match {
        case ('A', 3, y) => 3 - y
        case ('A', x, y) => y - 1 + Math.abs(x - 3) + 2
        case ('B', 5, y) => 3 - y
        case ('B', x, y) => y - 1 + Math.abs(x - 3) + 2
        case ('C', 7, y) => 3 - y
        case ('C', x, y) => y - 1 + Math.abs(x - 3) + 2
        case ('D', 9, y) => 3 - y
        case ('D', x, y) => y - 1 + Math.abs(x - 3) + 2
      }
    }
    def movingCosts(a: Amphipod, target: (Int, Int)): Int = {
      if (a._2 == target._1)
        costsPerMove(a._1) * Math.abs(a._3 - target._2)
      else
        costsPerMove(a._1) * (Math.abs(a._2 - target._1) + (a._3 - 1) + (target._2 - 1))
    }
    def neighbors(): List[Maze] = {
      val needsToMove = filterAmphipods.filterNot(isOnFinalLocation)
      needsToMove.flatMap(a => findPossibleLocations(a).map(moveAmphipod(a, _)))
    }
    def isOnFinalLocation(a: Amphipod): Boolean =
      a._2 == targetColumn(a._1) && (a._3 == 3 || (a._3 == 2 && maze(3)(a._2) == a._1))
    def findPossibleLocations(a: Amphipod): List[(Int, Int)] = {
      def horizontalMoves(x: Int): List[Int] = {
        ((x - 1 until 0 by -1).takeWhile(maze(1)(_) == '.').toList ++
          (x + 1 until 13).takeWhile(maze(1)(_) == '.').toList)
      }
      def intoTarget(x: Option[Int]): List[(Int, Int)] = {
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
      if (a._3 == 1) {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(hTargets.find(_ == targetColumn(a._1)))
      } else if (a._3 == 2) {
        val hTargets = horizontalMoves(a._2)
        hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(hTargets.find(_ == targetColumn(a._1)))
      } else { // a._3 == 2)
        if (maze(2)(a._2) == '.') {
          val hTargets = horizontalMoves(a._2)
          hTargets.filterNot(List(3, 5, 7, 9).contains).map((_, 1)) ++ intoTarget(hTargets.find(_ == targetColumn(a._1)))
        } else
          List.empty
      }
    }
    def moveAmphipod(a: Amphipod, target: (Int, Int)): Maze = {
      Maze(costs + movingCosts(a, target), updateMaze(a, target))
    }
    def updateMaze(a: Amphipod, target: (Int, Int)): List[String] = {
      val newMaze = maze.toArray.map(_.toArray)
      newMaze(a._3)(a._2) = '.'
      newMaze(target._2)(target._1) = a._1
      newMaze.map(_.mkString).toList
    }
  }

  def solveMaze(maze: Maze): Maze = {
    // A*
    @tailrec
    def solveMaze(visited: Set[List[String]], candidates: List[Maze]): Maze = {
      val current = candidates.filterNot(a => visited.contains(a.maze)).minBy(m => m.costs + m.expectedCosts)
//      current.print()
      if (current.maze == finalMaze)
        current
      else
        solveMaze(
          visited + current.maze,
          candidates.filterNot(_.eq(current)) ++ current.neighbors().filterNot(m => visited.contains(m.maze))
        )
    }
    solveMaze(Set.empty, List(maze))
  }

  override def solvePart1(lines: List[String]): Long = {
    // 18094 is too high
    // 17205 is too low
    solveMaze(Maze(0, lines)).costs
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day23 extends App {
  new Day23().solvePuzzles("/2021/day23.txt")
}
// 90+90+9000+600+7000+50+50+300+7+7+6+5
