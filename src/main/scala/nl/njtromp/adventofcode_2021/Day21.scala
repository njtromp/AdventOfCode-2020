package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day21 extends Puzzle {
  private val PlayerPosition = "Player (\\d) starting position: (\\d+)".r

  case class Player(id: Int, pos: Int, score: Long)

  sealed abstract class Dice(value: Int, rolls: Int) {
    def faceValue: Int = value
    def numberOrRolls: Int = rolls
    def roll: Dice
  }
  case class DiracDice(value: Int, rolls: Int) extends Dice(value, rolls) {
    def roll: Dice = DiracDice(if (value == 100) 1 else value + 1, numberOrRolls + 1)
  }

  private def readStartPositions(lines: List[String]): List[Player] = {
    lines.map {
      case PlayerPosition(id, pos) => Player(id.toInt, pos.toInt - 1, 0)
    }
  }

  @tailrec
  private def playDiracDice(players: List[Player], dice: Dice, winningScore: Int): (Dice, List[Player]) = {
    val activePlayer = players.head
    val rolls = (1 until 3).foldLeft(List(dice))((d, _) => d.head.roll :: d)
    val moves = rolls.map(_.faceValue).sum
    val newPos = (activePlayer.pos + moves) % 10
    val player = Player(activePlayer.id, newPos, activePlayer.score + newPos + 1)
    val playersForNextRound = players.tail ++ List(player)
    if (player.score >= winningScore)
      (rolls.head, playersForNextRound)
    else
      playDiracDice(playersForNextRound, rolls.head.roll, winningScore)
  }

  override def solvePart1(lines: List[String]): Long = {
    val players = readStartPositions(lines)
    val winningScore = 1000
    val result = playDiracDice(players, DiracDice(1, 1), winningScore)
    val losingPlayer = result._2.find(_.score < winningScore).get
    val dice = result._1
    dice.numberOrRolls *  losingPlayer.score
  }

  val diceToUniverses: Map[Int, Long] = Map(
    3 -> 1L,
    4 -> 3L,
    5 -> 6L,
    6 -> 7L,
    7 -> 6L,
    8 -> 3L,
    9 -> 1L
  )

  private val winningUniverses: Array[Long] = Array(0L, 0L)
  private def roleDice(activePlayer: Player, otherPlayer: Player, universes: Long, winningScore: Int): Unit = {
    if (otherPlayer.score >= winningScore) {
      winningUniverses(otherPlayer.id - 1) += universes
    } else {
      for (dice <- 3 to 9) {
        val pos = (activePlayer.pos + dice) % 10
        roleDice(otherPlayer, Player(activePlayer.id, pos, activePlayer.score + pos + 1), universes * diceToUniverses(dice), winningScore)
     }
    }
  }

  override def solvePart2(lines: List[String]): Long = {
    val players = readStartPositions(lines)
    val winningScore = 21
    roleDice(players.head, players.last, 1, winningScore)
    winningUniverses.max
  }
}

object Day21 extends App {
  new Day21().solvePuzzles("/2021/day21.txt")
}
