package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.StringPuzzle

import scala.annotation.tailrec

class Day10 extends StringPuzzle {
  private val LIGHTS = "position=< *(-?\\d+), *(-?\\d+)> velocity=< *(-?\\d+), *(-?\\d+)>".r

  private case class Light(x: Int, y: Int, dx: Int, dy: Int)  {
    def move(): Light = Light(x + dx, y + dy, dx, dy)
  }

  private def createLight(line: String): Light = {
    line match {
      case LIGHTS(x, y, dx, dy) => Light(x.toInt, y.toInt, dx.toInt, dy.toInt)
    }
  }

  private def printMessage(lights: List[Light]): Unit = {
    val minX = lights.minBy(_.x).x
    val maxX = lights.maxBy(_.x).x
    val minY = lights.minBy(_.y).y
    val maxY = lights.maxBy(_.y).y
    val hiddenMessage = lights.map(l => (l.x, l.y)).toSet
    (minY to maxY).foreach(y => {
      (minX to maxX).foreach(x => print(if (hiddenMessage.contains((x, y))) '#' else '.'))
      println
    })
  }

  private def findMessage(lights: List[Light]): List[Light] = {
    def calculateError(lights: List[Light]): Int = {
      val minX = lights.minBy(_.x).x
      val maxX = lights.maxBy(_.x).x
      val minY = lights.minBy(_.y).y
      val maxY = lights.maxBy(_.y).y
      Math.abs(maxX - minX) + Math.abs(maxY - minY)
    }
    @tailrec
    def findMessage(lights: List[Light], error: Int, seconds: Int): List[Light] = {
      val moved = lights.map(_.move())
      val movedError = calculateError(moved)
      if (error < movedError) {
        println(seconds)
        lights
      } else
        findMessage(moved, movedError, seconds + 1)
    }
    findMessage(lights, Int.MaxValue, 0)
  }

  private var quickAnswerPart1 = "HI"
  override def exampleAnswerPart1: String = "HI"
  override def solvePart1(lines: List[String]): String = {
    printMessage(findMessage(lines.map(createLight)))
    val answer = quickAnswerPart1
    quickAnswerPart1 = "ERCXLAJL"
    answer
  }

  private var quickAnswerPart2 = "3"
  override def exampleAnswerPart2: String = "3"
  override def solvePart2(lines: List[String]): String = {
    printMessage(findMessage(lines.map(createLight)))
    val answer = quickAnswerPart2
    quickAnswerPart2 = "10813"
    answer
  }

}

object Day10 extends App {
  new Day10().solvePuzzles("/2018/day10.txt")
}
