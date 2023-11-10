package nl.njtromp.adventofcode

class Day16 extends Puzzle[Long] with RouteFinding with Optimization {
  private val valveInfo = "Valve (\\w\\w) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z, ]+)".r

  private case class Valve(id: String, rate: Int, neighbors: List[String])

  override def exampleAnswerPart1: Long = 1651
  override def solvePart1(lines: List[String]): Long =
    val valves: List[Valve] = lines.map({case valveInfo(name, rate, connections) => Valve(name, rate.toInt, connections.split(",").map(_.trim).toList)})
    // Lookup based on ID
    val valveMapping = valves.map(v => v.id -> v).toMap
    // Only valves that contribute are worth moving to
    val usableValves = valves.filter(v => v.rate > 0)
    val startValve = valveMapping("AA")
    // Determine the traveling time between all usable valves including the starting valve
    val travelingTimes: Map[(String, String), Long] = (startValve :: usableValves).flatMap(start =>
      usableValves.filter(_ != start)
        .map(finish => (start.id, finish.id) -> (dijkstra[String](start.id, finish.id, v => valveMapping(v).neighbors, (_, _) => 1L).size - 1L))
    ).toMap
    val totalTime = 30L
    maximize[Valve](0L, totalTime, startValve, _ => usableValves, (v1, v2) => travelingTimes((v1.id, v2.id)), (v, t) => v.rate * (totalTime - t))

  override def exampleAnswerPart2: Long = 1707L
  override def solvePart2(lines: List[String]): Long =
    val valves: List[Valve] = lines.map({ case valveInfo(name, rate, connections) => Valve(name, rate.toInt, connections.split(",").map(_.trim).toList) })
    // Lookup based on ID
    val valveMapping = valves.map(v => v.id -> v).toMap
    // Only valves that contribute are worth moving to
    val usableValves = valves.filter(v => v.rate > 0)
    val startValve = valveMapping("AA")
    // Determine the traveling time between all usable valves including the starting valve
    val travelingTimes: Map[(String, String), Long] = (startValve :: usableValves).flatMap(start =>
      usableValves.filter(_ != start)
        .map(finish => (start.id, finish.id) -> (dijkstra[String](start.id, finish.id, v => valveMapping(v).neighbors, (_, _) => 1L).size - 1L))
    ).toMap
    val totalTime = 26L
    maximize[Valve](0L, totalTime, startValve, _ => usableValves, (v1, v2) => travelingTimes((v1.id, v2.id)), (v, t) => v.rate * (totalTime - t))

}

object Day16 extends App{
  new Day16().solvePuzzles("/day16.txt")
}
