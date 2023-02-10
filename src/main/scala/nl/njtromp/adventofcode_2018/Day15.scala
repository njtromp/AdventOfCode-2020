package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.{Puzzle3, SimpleMap, SimpleMapTypes}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day15 extends Puzzle3[Long] with SimpleMapTypes {
  private val ELF = 'E'
  private val GOBLIN = 'G'
  private def rOrder(pos: Pos): Int = pos._1 * 1000 + pos._2
  private class Entity(val kind: Char, var pos: Pos, var hitPoints: Int = 200, val attackPower: Int = 3) {
    def readingOrder: Int = rOrder(pos)
    def isAlive: Boolean = hitPoints > 0
    def moveTo(pos: Pos, maze: SimpleMap[Char]): Unit = {
      maze(this.pos) = '.'
      this.pos = pos
      maze(pos) = kind
    }
    override def toString: String = s"$kind($hitPoints)"
  }


  private def combat(maze: SimpleMap[Char], entities: List[Entity]): Int = {
    def dijkstra(start: Pos, target: Pos): (Int, Pos, Pos) = { // (length, move, target)
      class Info(val pos: Pos, var distance: Int = Int.MaxValue, var checked: Boolean = false, var origin: Pos = (-1, -1)) {
        override def toString: String = s"$distance $origin $checked"
      }
      def index(pos: Pos): Int = pos._1 * maze.width + pos._2
      val dijkstraInfo = maze.allPositions().map(new Info(_)).toArray
      dijkstraInfo(index(start)).distance = 1
      val needsInvestigation = ArrayBuffer.empty[Info]
      needsInvestigation.addOne(dijkstraInfo(index(start)))
      while (needsInvestigation.nonEmpty && needsInvestigation.head.pos != target) {
        val current = needsInvestigation.filterNot(_.checked).minBy(_.distance)
        needsInvestigation.remove(needsInvestigation.indexOf(current))
        if (!current.checked) {
          val newDistance = current.distance + 1
          maze.neighborPositions(current.pos, square).filter(maze(_) == '.').map(index).foreach(n => {
            if (newDistance < dijkstraInfo(n).distance) {
              dijkstraInfo(n).distance = newDistance
              dijkstraInfo(n).origin = current.pos
              needsInvestigation.addOne(dijkstraInfo(n))
            }
          })
          current.checked = true
        }
      }
      if (dijkstraInfo(index(target)).distance == Int.MaxValue) {
        (Int.MaxValue, (-1, -1), target)
      }
      else {
        (dijkstraInfo(index(target)).distance, start, target)
      }
    }
    @tailrec
    def round(roundNumber: Int): Int = {
      def weHaveAWinner: Boolean = entities.filter(_.isAlive).map(_.kind).toSet.size == 1
      def determineAttackableNeighbours(entity: Entity): Set[Pos] =
        maze.neighborPositions(entity.pos, square).filter(p => maze(p) != entity.kind && maze(p).isUpper).toSet
      def attackNeighbour(entity: Entity, attackablePositions: Set[Pos]): Unit = {
        val enemies = entities.filter(e => e.isAlive && attackablePositions.contains(e.pos))
        val enemyUnderAttack = enemies.groupBy(_.hitPoints).toList.minBy(_._1)._2.minBy(_.readingOrder)
        enemyUnderAttack.hitPoints -= entity.attackPower
        if (!enemyUnderAttack.isAlive) {
          maze(enemyUnderAttack.pos) = '.'
        }
      }
      if (weHaveAWinner)
        roundNumber
      else {
        var finished = false
        var fullRound = false
        entities.filter(_.isAlive).sortBy(_.readingOrder).foreach(entity => {
          fullRound = false
          if (!finished && entity.isAlive) {
            fullRound = true
            val attackablePositions = determineAttackableNeighbours(entity)
            if (attackablePositions.nonEmpty) {
              attackNeighbour(entity, attackablePositions)
              finished = weHaveAWinner
            } else {
              val possibleMoves = maze.neighborPositions(entity.pos, square).filter(maze(_) == '.')
              if (possibleMoves.nonEmpty) {
                val attackablePositions = entities.filter(e => e.isAlive && e.kind != entity.kind).flatMap(e => maze.neighborPositions(e.pos, square).filter(maze(_) == '.'))
                val shortestPaths = possibleMoves.flatMap(p => attackablePositions.map(dijkstra(p, _)))
                val possiblePositions = shortestPaths.filter(_._2 != (-1, -1))
                if (possiblePositions.nonEmpty) {
                  val nearestEnemies: List[(Pos, Pos)] = possiblePositions.groupBy(_._1).minBy(_._1)._2.map(p => (p._2, p._3))
                  val newPos: Pos = nearestEnemies.map(p => ((100_000 * rOrder(p._2)) + rOrder(p._1), p._1)).minBy(_._1)._2
                  entity.moveTo(newPos, maze)
                  val attackablePositions = determineAttackableNeighbours(entity)
                  if (attackablePositions.nonEmpty) {
                    attackNeighbour(entity, attackablePositions)
                    finished = weHaveAWinner
                  }
                }
              }
            }
          }
        })
        if (finished && !fullRound)
          roundNumber
        else
          round(roundNumber + 1)
      }
    }
    round(0)
  }

  override def exampleAnswerPart1 = 27730
  override def solvePart1(lines: List[String]): Long = {
    val maze = SimpleMap[Char](lines, _.toCharArray)
    val elfs = maze.allPositions().filter(maze(_) == ELF).map(new Entity(ELF, _))
    val goblins = maze.allPositions().filter(maze(_) == GOBLIN).map(new Entity(GOBLIN, _))
    val entities = elfs ++ goblins
    val rounds = combat(maze, entities)
    val remainingHitpoints = entities.filter(_.isAlive).map(_.hitPoints).sum
    rounds * remainingHitpoints
  }

  override def exampleAnswerPart2: Long = 4988
  override def solvePart2(lines: List[String]): Long = {
    var rounds = 0
    var remainingHitpoints = 0
    (4 to 20).find(attackPower => {
      val maze = SimpleMap[Char](lines, _.toCharArray)
      val elfs = maze.allPositions().filter(maze(_) == ELF).map(new Entity(ELF, _, attackPower = attackPower))
      val goblins = maze.allPositions().filter(maze(_) == GOBLIN).map(new Entity(GOBLIN, _))
      val entities = elfs ++ goblins
      rounds = combat(maze, entities)
      remainingHitpoints = entities.filter(_.isAlive).map(_.hitPoints).sum
      entities.count(e => e.isAlive && e.kind == ELF) == elfs.length
    })
    rounds * remainingHitpoints
  }

  private def printMaze(maze: SimpleMap[Char], enities: List[Entity]): Unit = {
    (0 until maze.height).foreach(y => {
      print(maze.elems(y).mkString)
      print("   ")
      println(enities.filter(e => e.isAlive && e.pos._1 == y).sortBy(_.readingOrder).mkString(", "))

    })
  }

}

object Day15 extends App {
  new Day15().solvePuzzles("/2018/day15.txt")
}
