package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day21 extends Puzzle {
  private val PlayerPosition = raw"Player (\d) starting position: (\d+)".r
  case class Player(id: Int, pos: Int, score: Int) {
    def move(dice: Dice): Player = {
      val newPos = pos + dice.faceValue % 100 + 1
      Player(id, newPos, score + newPos)
    }
  }
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
//    println(s"Player ${player.id} rolls ${rolls.reverse.map(_.faceValue).mkString("+")} and moves to space ${newPos + 1} for a total score of ${player.score}.")
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
//    println(s"Game finished dice: $dice and player $losingPlayer lost")
    dice.numberOrRolls *  losingPlayer.score
  }

  def pow(n: Long, p: Int): Long =
    if (p == 0) 1L else n * pow(n, p - 1)

  override def solvePart2(lines: List[String]): Long = {
    val players = readStartPositions(lines)
    val winningScore = 21
    val result = playDiracDice(players, DiracDice(1, 1), winningScore)
    val dice = result._1
    pow(3L*3L*3L, dice.numberOrRolls - 1)
  }
}

object Day21 extends App {
  new Day21().solvePuzzles("/2021/day21.txt")
}
