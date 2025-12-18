package nl.njtromp.adventofcode

import com.google.ortools.Loader
import com.google.ortools.linearsolver.{MPSolver, MPVariable}

class Day10 extends Puzzle[Long] {
  private val LAMP = '#'

  private def parse(line: String): (Set[Int], List[Set[Int]], Array[Int]) =
    def removeEdges(line: String): String = line.substring(1, line.length - 1)
    val splitted = line.split(' ')
    val lights = removeEdges(splitted.head).zipWithIndex.filter(_._1 == LAMP).map(_._2).toSet
    val buttons = splitted.tail.take(splitted.length - 2).map(removeEdges).map(_.split(',').map(_.toInt).toSet).toList
    val jotages = removeEdges(splitted.last).split(',').map(_.toInt).toArray
    (lights, buttons, jotages)

  private def switchAllLightOn(lights: Set[Int], buttons: List[Set[Int]]): Int =
    def toggleButtons(toggled: Set[Int], used: List[Set[Int]], candidates: List[Set[Int]]): List[List[Set[Int]]] =
      if candidates.isEmpty then
        List.empty
      else
        val toggle = candidates.head
        val common = toggled.intersect(toggle)
        val switchedLights = (toggled ++ toggle) -- common
        if switchedLights == lights then
          (toggle :: used) :: toggleButtons(toggled, used, candidates.tail)
        else
          toggleButtons(switchedLights, toggle :: used, candidates.tail) ++ toggleButtons(toggled, used, candidates.tail)
    val toggledCombinations = toggleButtons(Set.empty, List.empty, buttons)
    toggledCombinations.map(_.size).min

  private def powerRequirements(buttons: List[Set[Int]], targetJoltages: Array[Int]): Long =
    val solver = MPSolver.createSolver("SCIP")
    val buttonPresses = buttons.indices.map(i => solver.makeIntVar(0, Double.PositiveInfinity, s"b$i")).toArray
    targetJoltages.indices.foreach(i =>
      val constraint = solver.makeConstraint(targetJoltages(i), targetJoltages(i))
      buttons.indices.foreach(j =>
        constraint.setCoefficient(buttonPresses(j), if buttons(j).contains(i) then 1 else 0)
      )
    )
    val objective = solver.objective()
    buttonPresses.foreach(b => objective.setCoefficient(b, 1))
    objective.minimization()
    val status = solver.solve()
    if status != MPSolver.ResultStatus.OPTIMAL then
      println("THE SOLUTION IS NOT OPTIMAL!")
    buttonPresses.map(_.solutionValue().toLong).sum

  override def exampleAnswerPart1: Long = 7
  override def solvePart1(lines: List[String]): Long =
    lines.map(line =>
      val (l, b, _) = parse(line)
      switchAllLightOn(l, b)
    ).sum

  override def exampleAnswerPart2: Long = 33
  override def solvePart2(lines: List[String]): Long =
    lines.map(line =>
      val (_, b, j) = parse(line)
      powerRequirements(b, j)
    ).sum

}

object Day10 extends App {
  Loader.loadNativeLibraries()
  new Day10().solvePuzzles()
}
