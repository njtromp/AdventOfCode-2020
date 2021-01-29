package nl.njtromp.adventofcode_2020

import scala.collection.mutable
import scala.io.Source

object Day06 extends App {
  private var positiveQuestionsPerGroup: mutable.Map[Char, Int] = mutable.Map.empty.withDefaultValue(0)
  private var listOfAllPositiveQuestions: List[mutable.Map[Char, Int]] = List.empty
  private var groupSize = 0
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/2020/input-puzzle06.txt")).getLines) {
    if (line.isEmpty) {
      positiveQuestionsPerGroup += ('_' -> groupSize)
      listOfAllPositiveQuestions = positiveQuestionsPerGroup +: listOfAllPositiveQuestions
      positiveQuestionsPerGroup = mutable.Map.empty.withDefaultValue(0)
      groupSize = 0
    } else {
      groupSize += 1
      line.chars().forEach(c => positiveQuestionsPerGroup += (c.toChar -> (1 + positiveQuestionsPerGroup(c.toChar))))
    }
  }
  println(s"Answer part 1: ${listOfAllPositiveQuestions.map(g => g.keySet.size).sum}")
  println(s"Answer part 2: ${listOfAllPositiveQuestions.flatMap(p => p.filterKeys(k => !'_'.equals(k) && p.get(k) == p.get('_'))).size}")
}
