package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day22 extends Puzzle {

  private val CubesOnOff = raw"(.+) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r
  private val InitRange = (-50 to 50)
  private val InitCube = Cube(InitRange, InitRange, InitRange)

  case class Cube(x: Range, y: Range, z: Range) {
    private def overlapping(a: Range, b: Range): Range = {
      val overlap = a.intersect(b)
      (overlap.min to overlap.max)
    }
    def size: BigInt = x.size.toLong * y.size.toLong * z.size.toLong
    def points: Set[(Int, Int, Int)] = x.flatMap(x => y.flatMap(y => z.map((x, y, _)))).toSet
    def overlap(c : Cube): Boolean = {
      x.intersect(c.x).nonEmpty &&
      y.intersect(c.y).nonEmpty &&
      z.intersect(c.z).nonEmpty
    }
    def contains(c: Cube): Boolean = {
      x.min <= c.x.min && x.max >= c.x.max &&
        y.min <= c.y.min && y.max >= c.y.max &&
        z.min <= c.z.min && z.max >= c.z.max
    }
    def overlapping(c: Cube): Cube = Cube(overlapping(x, c.x), overlapping(y, c.y), overlapping(z, c.z))
  }
  case class Instruction(ins: String, cube: Cube)

  private def readInstructions(lines: List[String]): List[Instruction] = {
    lines.filterNot(_.isBlank).map({
      case CubesOnOff(ins, minX, maxX, minY, maxY, minZ, maxZ) => Instruction(
        ins, Cube(
          minX.toInt to maxX.toInt,
          minY.toInt to maxY.toInt,
          minZ.toInt to maxZ.toInt
        )
      )
    })
  }

  def startReactor(instructions: List[Instruction]): BigInt = {
    @tailrec
    def startReactor(activeCubes: BigInt, processedInstruction: List[Instruction], instructions: List[Instruction]): BigInt = {
      if (instructions.isEmpty)
        activeCubes
      else {
        val instruction = instructions.head
        val cube = instruction.cube
        val activatedCubes = instruction.ins match {
          case "on" => instruction.cube.size - processedInstruction.filterNot(_.cube.overlap(cube)).map(o =>
            if (o.cube.overlap(cube))
              o.cube.overlapping(cube).size
            else
              BigInt(0)
            ).sum
          case "off" => BigInt(0) - processedInstruction.map(o =>
            if (o.cube.overlap(cube))
              o.cube.overlapping(cube).size
            else
              BigInt(0)
          ).sum
        }
        startReactor(activeCubes + activatedCubes, processedInstruction ++ List(instruction), instructions.tail)
      }
    }
    startReactor(0, List.empty, instructions)
  }

  override def solvePart1(lines: List[String]): Long = {
    val instructions = readInstructions(lines).filter(_.cube.overlap(InitCube))
//    590784
    startReactor(instructions).toLong
  }

  override def solvePart2(lines: List[String]): Long = {
    val instructions = readInstructions(lines)
    startReactor(instructions).toLong
  }
}

object Day22 extends App {
  new Day22().solvePuzzles("/2021/day22.txt")
}
