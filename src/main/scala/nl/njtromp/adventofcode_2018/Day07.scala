package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.StringPuzzle

import scala.annotation.tailrec
import scala.collection.SortedSet

class Day07 extends StringPuzzle {
  private val order = "Step (\\w) must be finished before step (\\w) can begin.".r

  private def performSteps(possibleSteps: SortedSet[String], stepOrder: Map[String, SortedSet[String]], requirements: Map[String, SortedSet[String]]): String = {
    @tailrec
    def step(possibleSteps: SortedSet[String], stepsPerformed: Set[String], order: String): String = {
      if (possibleSteps.isEmpty)
        order
      else {
        val stepToBePerformed = possibleSteps.head
        val newStepsPerformed = stepsPerformed + stepToBePerformed
        step(
          possibleSteps.tail ++ stepOrder(stepToBePerformed).filter(s => !order.contains(s) && requirements(s).subsetOf(newStepsPerformed)),
          newStepsPerformed,
          order + stepToBePerformed
        )
      }
    }

    step(possibleSteps, Set(possibleSteps.head), "")
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
    performSteps(SortedSet.from(stepOrder.keySet diff stepOrder.flatMap(_._2).toSet), stepOrder, requirements)
  }

  override def exampleAnswerPart2: String = "nothing"
  override def solvePart2(lines: List[String]): String = {
    "bla bla"
  }

}

object Day07 extends App {
  new Day07().solvePuzzles("/2018/day07.txt")
}
