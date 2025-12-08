package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day08 extends Puzzle[Long] {
  private type JunctionBox = (Long, Long, Long)
  private type Connection = (JunctionBox, JunctionBox)

  extension (jb: JunctionBox)
    def distance(o: JunctionBox): Double =
      val dX = jb._1 - o._1
      val dY = jb._2 - o._2
      val dZ = jb._3 - o._3
      Math.sqrt(dX * dX + dY * dY + dZ * dZ)

  private def createOrderedConnections(junctionBoxes: List[JunctionBox]): List[Connection] =
    @tailrec
    def createConnections(junctionBoxes: List[JunctionBox], connections: List[Connection]): List[Connection] =
      if junctionBoxes.size <= 1 then
        connections
      else
        createConnections(junctionBoxes.tail, junctionBoxes.tail.map((junctionBoxes.head, _)) ++ connections)
    createConnections(junctionBoxes, List.empty).sortWith((c1, c2) => c1._1.distance(c1._2) < c2._1.distance(c2._2))

  private def connectClosestJunctionBoxes(connection: Connection, circuits: Set[Set[JunctionBox]]): Set[Set[JunctionBox]] =
    val (jb1, jb2) = connection
    val matchingCircuits = circuits.filter(c => c.contains(jb1) || c.contains(jb2))
    val newCircuits = if matchingCircuits.isEmpty then
      Set(Set(jb1, jb2))
    else if matchingCircuits.size == 1 then
      Set(Set(jb1, jb2) ++ matchingCircuits.head)
    else // matchingCircuits.size == 2
      Set(matchingCircuits.head ++ matchingCircuits.last)
    circuits.diff(matchingCircuits) ++ newCircuits

  private def createCircuits(nrOfConnections: Int, junctionBoxes: List[JunctionBox]): Set[Set[JunctionBox]] =
    @tailrec
    def connectClosest(nrOfConnections: Int, closest: List[Connection], circuits: Set[Set[JunctionBox]]): Set[Set[JunctionBox]] =
      if nrOfConnections == 0 then
        circuits
      else
        val newCircuits = connectClosestJunctionBoxes(closest.head, circuits)
        connectClosest(nrOfConnections - 1, closest.tail, newCircuits)
    connectClosest(nrOfConnections, createOrderedConnections(junctionBoxes), Set.empty)

  private def findConnectingJunctionBoxes(junctionBoxes: List[JunctionBox]): Connection =
    @tailrec
    def connectClosest(closest: List[Connection], circuits: Set[Set[JunctionBox]]): Connection =
      val newCircuits = connectClosestJunctionBoxes(closest.head, circuits)
      if newCircuits.head.size == junctionBoxes.size then
        closest.head
      else
        connectClosest(closest.tail, newCircuits)
    connectClosest(createOrderedConnections(junctionBoxes), Set.empty)

  override def exampleAnswerPart1: Long = 40
  override def solvePart1(lines: List[String]): Long =
    val nrOfConnections = if lines.size < 50 then 10 else 1000
    val junctionBoxes = lines.map {case s"$x,$y,$z" => (x.toLong, y.toLong, z.toLong)}
    val circuits = createCircuits(nrOfConnections, junctionBoxes)
    circuits.map(_.size).toList.sorted.reverse.take(3).product

  override def exampleAnswerPart2: Long = 25272
  override def solvePart2(lines: List[String]): Long =
    val junctionBoxes = lines.map {case s"$x,$y,$z" => (x.toLong, y.toLong, z.toLong)}
    val finalConnection = findConnectingJunctionBoxes(junctionBoxes)
    finalConnection._1._1 * finalConnection._2._1

}

object Day08 extends App {
  new Day08().solvePuzzles()
}
