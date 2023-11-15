package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.StringPuzzle

import scala.annotation.tailrec
import scala.collection.SortedSet

class Day07 extends StringPuzzle {
  private val order = "Step (\\w) must be finished before step (\\w) can begin.".r

  private def performSteps(possibleSteps: SortedSet[String], stepOrder: Map[String, SortedSet[String]], requirements: Map[String, SortedSet[String]]): String = {
    @tailrec
    def step(possibleSteps: SortedSet[String], stepsPerformed: Set[String], stepsPerformedInOrder: String): String =
      if (possibleSteps.isEmpty)
        stepsPerformedInOrder
      else {
        val stepToBePerformed = possibleSteps.head
        val newStepsPerformed = stepsPerformed + stepToBePerformed
        step(
          possibleSteps.tail ++ stepOrder(stepToBePerformed).diff(newStepsPerformed).filter(s => requirements(s).subsetOf(newStepsPerformed)),
          newStepsPerformed,
          stepsPerformedInOrder + stepToBePerformed
        )
      }
    step(possibleSteps, Set(possibleSteps.head), "")
  }

  private def sorter(a: (Int, String)): Int = if (a._2.isEmpty) Int.MaxValue else a._1
  private def sorter(a: (Int, String), b: (Int, String)): Boolean = sorter(a) < sorter(b)

  private def performStepsSimultaneous(possibleSteps: SortedSet[String], stepOrder: Map[String, SortedSet[String]], requirements: Map[String, SortedSet[String]]): Long = {
    def spreadTasks(time: Int, workers: List[(Int, String)], steps: SortedSet[String]): (List[(Int, String)], SortedSet[String]) = {
      (workers, steps.isEmpty) match {
        case (Nil, _) => (workers, steps) // No more workers
        case (_, true) => (workers, steps) // No more tasks
        case ((0, "") :: tail, _) => // Idle worker
          val (a, b) = spreadTasks(time, tail, steps.tail)
          ((time + Day07.timePerStep + (steps.head.head - 'A' + 1), steps.head) :: a, b)
        case (head :: tail, _) => // Busy worker, try next worker(s)
          val (a, b) = spreadTasks(time, tail, steps)
          (head :: a, b)
      }
    }
    @tailrec
    def step(workers: List[(Int, String)], possibleSteps: SortedSet[String], stepsPerformed: Set[String], time: Int): Long =
      if (possibleSteps.isEmpty && workers.forall(_._2.isEmpty))
        time
      else {
        val (stepTime, stepToBePerformed) = workers.head
        val newStepsPerformed = stepsPerformed + stepToBePerformed
        val (newWorkers, newPossibleSteps) = spreadTasks(stepTime, (0, "") :: workers.tail, possibleSteps ++ stepOrder(stepToBePerformed).diff(newStepsPerformed).filter(s => requirements(s).subsetOf(newStepsPerformed)))
        step(newWorkers.sortWith(sorter), newPossibleSteps, newStepsPerformed, stepTime)
      }

    val (workers, unassignedSteps) = spreadTasks(0, (0 until Day07.numberOfWorkers).map(_ => (0, "")).toList, possibleSteps)
    step(workers.sortWith(sorter), unassignedSteps, Set.empty[String], 0)
  }

  override def exampleAnswerPart1: String = "CABDFE"
  override def solvePart1(lines: List[String]): String = {
    val emptyMapWithEmptySets = Map.empty[String, SortedSet[String]].withDefaultValue(SortedSet.empty[String])
    val stepOrder = lines.foldLeft(emptyMapWithEmptySets)((stepOrder, step) => step match {
      case order(parent, child) => stepOrder + (parent -> (stepOrder(parent) + child))
    })
    val requirements = lines.foldLeft(emptyMapWithEmptySets)((dependencies, step) => step match {
      case order(parent, child) => dependencies + (child -> (dependencies(child) + parent))
    })
    val startingSteps = SortedSet.from(stepOrder.keySet diff stepOrder.flatMap(_._2).toSet)
    performSteps(startingSteps, stepOrder, requirements)
  }

  override def exampleAnswerPart2: String = "15"
  override def solvePart2(lines: List[String]): String = {
    val emptyMapWithEmptySets = Map.empty[String, SortedSet[String]].withDefaultValue(SortedSet.empty[String])
    val stepOrder = lines.foldLeft(emptyMapWithEmptySets)((stepOrder, step) => step match {
      case order(parent, child) => stepOrder + (parent -> (stepOrder(parent) + child))
    })
    val requirements = lines.foldLeft(emptyMapWithEmptySets)((dependencies, step) => step match {
      case order(parent, child) => dependencies + (child -> (dependencies(child) + parent))
    })
    val startingSteps = SortedSet.from(stepOrder.keySet diff stepOrder.flatMap(_._2).toSet)
    val timeNeeded  = performStepsSimultaneous(startingSteps, stepOrder, requirements)
    // Prepare for final solution
    Day07.numberOfWorkers = 5
    Day07.timePerStep = 60
    timeNeeded.toString
  }

}

object Day07 extends App {
  private var numberOfWorkers: Int = 2
  private var timePerStep: Int = 0
  new Day07().solvePuzzles("/2018/day07.txt")
}
