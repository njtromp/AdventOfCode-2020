package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day08 extends Puzzle[Long] {
  private type JB = (Long, Long, Long)
  private type Connection = (JB, JB)

  extension (jb: JB)
    def distance(o: JB): Double =
      val dX = jb._1 - o._1
      val dY = jb._2 - o._2
      val dZ = jb._3 - o._3
      Math.sqrt(dX * dX + dY * dY + dZ * dZ)

  private def findClosest(junctionBoxes: List[JB]): List[Connection] =
    @tailrec
    def createConnections(junctionBoxes: List[JB], connections: List[Connection]): List[Connection] =
      if junctionBoxes.size <= 1 then
        connections
      else
        val first = junctionBoxes.head
        createConnections(junctionBoxes.tail, junctionBoxes.tail.map((first, _)) ++ connections)
    createConnections(junctionBoxes, List.empty).sortWith((c1, c2) => c1._1.distance(c1._2) < c2._1.distance(c2._2))

  private def createCircuits(nrOfConnections: Int, junctionBoxes: List[JB]): Set[Set[JB]] =
    @tailrec
    def connectClosest(nrOfConnections: Int, closest: List[Connection], circuits: Set[Set[JB]]): Set[Set[JB]] =
      if nrOfConnections == 0 then
        circuits
      else
        val connection = closest.head
        val jb1 = connection._1
        val jb2 = connection._2
        val matchingCircuits = circuits.filter(c => c.contains(jb1) || c.contains(jb2))
        val newCircuits: Set[Set[JB]] = if matchingCircuits.isEmpty then
          Set(Set(jb1, jb2))
        else if matchingCircuits.size == 1 then
          Set(Set(jb1, jb2) ++ matchingCircuits.head)
        else if matchingCircuits.size == 2 then
          Set(matchingCircuits.head ++ matchingCircuits.last)
        else
          println("This should not happen!")
          Set.empty
        val remainingConnections = if newCircuits == circuits then nrOfConnections else nrOfConnections - 1
        connectClosest(remainingConnections, closest.tail, circuits.diff(matchingCircuits) ++ newCircuits)
    connectClosest(nrOfConnections, findClosest(junctionBoxes), Set.empty)

  private def findConnectingJunctionBoxes(junctionBoxes: List[JB]): Connection =
    @tailrec
    def connectClosest(closest: List[Connection], circuits: Set[Set[JB]]): Connection =
      val connection = closest.head
      val jb1 = connection._1
      val jb2 = connection._2
      val matchingCircuits = circuits.filter(c => c.contains(jb1) || c.contains(jb2))
      val newCircuits: Set[Set[JB]] = if matchingCircuits.isEmpty then
        Set(Set(jb1, jb2))
      else if matchingCircuits.size == 1 then
        Set(Set(jb1, jb2) ++ matchingCircuits.head)
      else if matchingCircuits.size == 2 then
        Set(matchingCircuits.head ++ matchingCircuits.last)
      else
        println("This should not happen!")
        Set.empty
      val combinedCircuits = circuits.diff(matchingCircuits) ++ newCircuits
      if combinedCircuits.head.size == junctionBoxes.size then
        (jb1, jb2)
      else
        connectClosest(closest.tail, combinedCircuits)
    connectClosest(findClosest(junctionBoxes), Set.empty)

  override def exampleAnswerPart1: Long = 40
  override def solvePart1(lines: List[String]): Long =
    val nrOfConnections = if lines.size < 50 then 10 else 1000
    val junctionBoxes = lines.map {
      case s"$x,$y,$z" => (x.toLong, y.toLong, z.toLong)
    }
    val circuits = createCircuits(nrOfConnections, junctionBoxes)
    circuits.map(_.size).toList.sorted.reverse.take(3).product

  override def exampleAnswerPart2: Long = 25272
  override def solvePart2(lines: List[String]): Long =
    val junctionBoxes = lines.map {
      case s"$x,$y,$z" => (x.toLong, y.toLong, z.toLong)
    }
    val finalConnection = findConnectingJunctionBoxes(junctionBoxes)
    finalConnection._1._1 * finalConnection._2._1

}

object Day08 extends App {
  new Day08().solvePuzzles()
}
