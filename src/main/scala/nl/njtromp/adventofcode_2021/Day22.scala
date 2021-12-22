package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day22 extends Puzzle {

  private val CubesOnOff = raw"(.+) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r
  private val InitRange = (-50 to 50)

  case class Instruction(ins: String, x: Range, y: Range, z: Range)

  private def readInstructions(lines: List[String]): List[Instruction] = {
    lines.filterNot(_.isBlank).map({
      case CubesOnOff(ins, minX, maxX, minY, maxY, minZ, maxZ) => Instruction(
        ins,
        minX.toInt to maxX.toInt,
        minY.toInt to maxY.toInt,
        minZ.toInt to maxZ.toInt
      )
    })
  }

  def startReactor(instructions: List[Instruction]): Set[(Int, Int, Int)] = {
    @tailrec
    def startReactor(cubes: Set[(Int, Int, Int)], instructions: List[Instruction]): Set[(Int, Int, Int)] = {
      print('.')
      if (instructions.isEmpty)
        cubes
      else {
        val newCubes = instructions.head match {
          case Instruction(ins, x, y, z) => {
            val cub = x.flatMap(x => y.flatMap(y => z.map((x, y, _))))
            ins match {
              case "on" => cubes ++ cub
              case "off" => cubes -- cub
            }
          }
        }
        startReactor(newCubes, instructions.tail)
      }
    }
    startReactor(Set.empty, instructions)
  }

  override def solvePart1(lines: List[String]): Long = {
    val instructions = readInstructions(lines)
      .filter(i => i.x.intersect(InitRange).nonEmpty && i.y.intersect(InitRange).nonEmpty && i.z.intersect(InitRange).nonEmpty)
    val cubes = startReactor(instructions)
    InitRange.flatMap(x => InitRange.flatMap(y => InitRange.map(z => (x, y, z)))).count(cubes.contains)
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day22 extends App {
  new Day22().solvePuzzles("/2021/day22.txt")
}
