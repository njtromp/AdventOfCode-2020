package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day25 extends Puzzle[Long] {
  private case class Node(name: String) {
    val neighbors: ListBuffer[Node] = ListBuffer.empty
    def connectTo(neighbor: Node): Unit =
      neighbors += neighbor
      neighbor.neighbors += this
    def disconnectFrom(neighbor: Node): Unit =
      neighbors -= neighbor
      neighbor.neighbors -= this
    override def toString: String = s"Node($name):${neighbors.size}"
  }
  private case class Edge(between: (Node, Node)) {
    override def equals(obj: Any): Boolean = obj match {
      case Edge(edge) =>
        (between._1 == edge._1 && between._2 == edge._2) ||
        (between._1 == edge._2 && between._2 == edge._1)
      case _ => false
    }
    // The 'order' of the nodes should not matter, hence we add the hashcodes.
    override def hashCode(): Int = between._1.hashCode() + between._2.hashCode()
    override def toString: String = s"Edge(${between._1.name},${between._2.name})"
  }

  private def parseLine(line: String): List[Node] =
    line.replaceAll(":", "").split(" ").map(l => Node(l.trim)).toList

  private def connect(nodesMap: Map[String, Node], lines: List[String]): Unit =
    lines.foreach(line =>
     val nodes = line.replaceAll(":", "").split(" ")
     nodes.tail.foreach(nodesMap(_).connectTo(nodesMap(nodes.head)))
    )

  private def countConnectedNodes(root: Node): Long =
    val toBeVisited = mutable.Queue(root)
    val connected = mutable.Set.empty[Node]
    while toBeVisited.nonEmpty do
      val current = toBeVisited.dequeue()
      connected += current
      toBeVisited.enqueueAll(current.neighbors.toSet diff connected)
    connected.size

  private def registerEdgeDistances(source: Node): List[(Edge, Int)] =
    val toBeVisited = mutable.Queue(source)
    val visited = mutable.Set.empty[Node]
    val edgeDistances = ListBuffer.empty[(Edge, Int)]
    var distanceFromSource = 0
    while toBeVisited.nonEmpty do
      val current = toBeVisited.dequeue()
      visited += current
      val neighbors = (current.neighbors.toSet diff visited) diff toBeVisited.toSet
      neighbors.foreach(n =>
        edgeDistances += ((Edge(current, n), distanceFromSource))
        toBeVisited.enqueue(n)
      )
      distanceFromSource += 1
    edgeDistances.toList

  private def split(edges: List[Edge]): Long =
    var score = 0L
    edges.indices.foreach(i1 =>
      val e1 = edges(i1)
      e1.between._1.disconnectFrom(e1.between._2)
      (i1 + 1 until edges.size).foreach(i2 =>
        val e2 = edges(i2)
        e2.between._1.disconnectFrom(e2.between._2)
        (i2 + 1 until edges.size).foreach(i3 =>
          val e3 = edges(i3)
          e3.between._1.disconnectFrom(e3.between._2)
          val size1 = countConnectedNodes(e3.between._1)
          val size2 = countConnectedNodes(e3.between._2)
          if size1 > 0 && size2 > 0 && size1 != size2 then
            println(s"$e1 $e2 $e3")
            score = size1 * size2
          e3.between._1.connectTo(e3.between._2)
        )
        e2.between._1.connectTo(e2.between._2)
      )
      e1.between._1.connectTo(e1.between._2)
    )
    score

  override def exampleAnswerPart1: Long = 54
  override def solvePart1(lines: List[String]): Long =
    val nodes = lines.flatMap(parseLine)
    val namedNodes = nodes.map(n => n.name -> n).toMap
    connect(namedNodes, lines)
    val edgeDistances = nodes
      .flatMap(registerEdgeDistances) // Using BFS to determine the distance for each edge from every node
      .groupBy(_._1) // Gather the distances per edge
      .toList // Candidate edges will most likely are reachable from the most nodes
      .sortBy(-_._2.size) // Sort descending on the number of edges that are reachable
    // Take the top-4 and see what happens if we disconnect three of them
    // For the example we need the top-4, for the real thing we need the top-3
    edgeDistances.take(4).map(_._1).foreach(println)
    split(edgeDistances.take(4).map(_._1))

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day25 extends App {
  new Day25().solvePuzzles()
}
