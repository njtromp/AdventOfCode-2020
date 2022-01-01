package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day22 extends Puzzle {

  private val CubesOnOff = raw"(.+) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r
  private val InitRange = -50 to 50
  private val InitCube = Cube(InitRange, InitRange, InitRange)

  case class Cube(x: Range, y: Range, z: Range) {
    def size: Long = x.size.toLong * y.size.toLong * z.size.toLong
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
    def intersect(c: Cube): Cube = Cube(overlapping(x, c.x), overlapping(y, c.y), overlapping(z, c.z))
    private def overlapping(a: Range, b: Range): Range = {
      val overlap = a.intersect(b)
      overlap.min to overlap.max
    }
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

  def parts(outer: Range, inner: Range): List[Range] =
    List(outer.min until inner.min, inner.min to inner.max, inner.max + 1 to outer.max).filterNot(_.isEmpty)

  private var instructionCounter = 1
  def performInstruction(onCubes: List[Cube], instruction: Instruction): List[Cube]= {
    println(s"Instruction $instructionCounter")
    instructionCounter += 1
    val newCube = instruction.cube
    val noneOverlapping = onCubes.filterNot(newCube.overlap)
    // If there is an overlap the original cube is split into multiple smaller cubes that don't overlap and a single
    // cube that is exists in both the original and the new cube. The smaller cubes replace the original cube excluding
    // the overlapping (smaller) cube. At this moment we don't have to take into account the type of instruction.
    val overlapping = onCubes.filter(newCube.overlap).flatMap(c => {
      if (newCube.contains(c)) {
        List.empty // The contained cube will either be removed (off) or replaced (on) so it will always be removed
      } else {
        val intersection = newCube.intersect(c)
        // Chop along each axis into at most 3 ranges and combine that into at most 27 partial cubes ...
        // ... and remove the overlapping (intersection) partial cube.
        // If the instruction is a 'on' instruction the intersected part will be included by newCube
        val xParts = parts(c.x, intersection.x)
        val yParts = parts(c.y, intersection.y)
        val zParts = parts(c.z, intersection.z)
        xParts.flatMap(x => yParts.flatMap(y => zParts.map(z => Cube(x, y, z)))).filterNot(_ == intersection)
      }
    })
    if (instruction.ins == "on") {
      // Add the new cube to the list of cubes, if it overlapped with one or more other cubes the overlapping part has
      // been removed so we can safely add the whole cube
      newCube :: overlapping ++ noneOverlapping
    } else {
      overlapping ++ noneOverlapping
    }
  }

  def startReactor(instructions: List[Instruction]): List[Cube] = {
    instructions.foldLeft(List.empty[Cube])(performInstruction)
  }

  override def solvePart1(lines: List[String]): Long = {
    val instructions = readInstructions(lines).filter(_.cube.overlap(InitCube))
    startReactor(instructions).map(_.size).sum
  }

  override def solvePart2(lines: List[String]): Long = {
    val instructions = readInstructions(lines)
    startReactor(instructions).map(_.size).sum
  }
}

object Day22 extends App {
  new Day22().solvePuzzles("/2021/day22.txt")
}
