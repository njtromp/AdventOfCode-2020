package nl.njtromp.adventofcode

class Day06 extends Puzzle[Long] with RouteFinding {

  private def split(line: String): (String, String) =
    val splitted = line.split(')')
    (splitted.last, splitted.head)

  private def numberOfOrbits(planet: String, orbits: Map[String, String]): Long =
    if orbits.contains(planet) then 1 + numberOfOrbits(orbits(planet), orbits) else 0

  override def exampleAnswerPart1: Long = 54 // 42
  override def solvePart1(lines: List[String]): Long =
    val orbiting = lines.map(split).toMap
    orbiting.keys.toList.map(k => numberOfOrbits(k, orbiting)).sum

  override def exampleAnswerPart2: Long = 4
  override def solvePart2(lines: List[String]): Long =
    val orbiting = lines.map(split).toMap
    def neighbours(current: String): List[String] =
      val orbitingCurrent = orbiting.keys.filter(p => orbiting(p) == current).toList
        if orbiting.contains(current) then
          orbiting(current) :: orbitingCurrent
        else
          orbitingCurrent

    val route = bfs("YOU", "SAN", neighbours)
    route.size - 3

}

object Day06 extends App {
  new Day06().solvePuzzles()
}
