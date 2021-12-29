package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.{Matrix, Puzzle}

import scala.util.matching.Regex

class Day19 extends Puzzle {

  private val ScannerLine: Regex = raw"--- scanner (\d+) ---".r
  private type Pos = List[Int]

  case class Scanner(id: Int, beacons: Set[Matrix]) {
    def intersect(s: Scanner): Set[Matrix] = beacons.intersect(s.beacons)
    def relativeLocations: Set[Matrix] = {
      val beaconList = beacons.toList
      val relativeBeacons: List[Matrix] = beaconList.flatMap(b0 =>
        beaconList.map(b => b - b0)
      )
      relativeBeacons.toSet
    }
    def transform(t: Matrix): Scanner = {
      Scanner(id, beacons.map(b => t * b))
    }
  }

  private def parseScannerInfo(lines: List[String]): List[Scanner] = {
    lines.foldLeft(List.empty[Scanner], List.empty[Pos])((acc, l) => l match {
      case ScannerLine(id) => (Scanner(id.toInt, Set.empty[Matrix]) :: acc._1, List.empty[Pos])
      case "" =>
        val scanner = acc._1.head
        val beacons = acc._2.map(p => Matrix(List(p.map(_.toDouble))).rowsToColums).toSet
        (Scanner(scanner.id, beacons) :: acc._1.tail, List.empty[Pos])
      case _ =>
        val pos = l.split(",").map(_.toInt).toList
        (acc._1, List(pos) ++ acc._2)
    })._1
  }

  def sameDistance(b1_1: Matrix, b1_2: Matrix, b2_1: Matrix, b2_2: Matrix): Boolean = b1_1 - b1_2 == b2_1 - b2_2

  def findMatching(other: List[Matrix], b1_1: Matrix, b1_2: Matrix): List[(Matrix, Matrix)] = {
    other.flatMap(b2_1 => other.filterNot(b2_1.eq(_)).filter(b2_2 => sameDistance(b1_1, b1_2, b2_1, b2_2)).map((b2_1, _)))
  }

  def haveSameBeacons(beacons: List[Matrix], others: List[Matrix]): List[((Matrix, Matrix), (Matrix, Matrix))] = {
    beacons
      .flatMap(b1_1 => beacons.filterNot(b1_1.eq(_)).map(b1_2 => ((b1_1, b1_2), findMatching(others, b1_1, b1_2))))
      .filter(_._2.nonEmpty)
      .map(m => (m._1, m._2.head))
  }

  def findTransformations(base: Scanner, scanners: List[Scanner]): List[(Scanner, Scanner, Option[Matrix])] = {
    scanners.filterNot(_ == base).map(s => {
      (base, s, Matrix.perpendicularPermutations.find(p => {
        val mappedBeacons = haveSameBeacons(base.beacons.toList, s.beacons.map(p * _).toList)
        if (mappedBeacons.size >= 24) {
          println(s"${base.id} -> ${s.id}")
          mappedBeacons.foreach(m => {
            println(s"${(m._2._1 - m._1._1).toTuple}")
            println(s"${(m._2._2 - m._1._2).toTuple}")
          })
//          mappedBeacons
//            .map(m => ((m._1._1.toTuple, m._1._2.toTuple), (m._2._1.toTuple, m._2._2.toTuple)))
//            .foreach(m => println(s"${m._1} -> ${m._2}"))
        }
        mappedBeacons.size >= 24
      }))
    })
  }

  def findScannerMappings(connectedScanners: Set[Int], needInvestigation: List[Int], scanners: List[Scanner]): List[(Int, Int, Matrix)] = {
    if (needInvestigation.isEmpty)
      List.empty
    else {
      val base = scanners.find(_.id == needInvestigation.head).get
      val unmapped = scanners.filterNot(s => connectedScanners.contains(s.id) || needInvestigation.contains(s.id))
      val matchedScanners = findTransformations(base, unmapped)
        .filter(_._3.isDefined)
        .map(s => (s._1, s._2, s._3.get))
        .map(s => (s._1, s._2.transform(s._3), s._3))
      val findConnectionsFor = (needInvestigation.tail ++ matchedScanners.map(_._2.id)).filterNot(connectedScanners.contains)
      matchedScanners.map(s => (s._1.id, s._2.id, s._3)) ++
        findScannerMappings(connectedScanners + needInvestigation.head, findConnectionsFor, scanners)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val scanners = parseScannerInfo(lines).reverse
    val transformations = findScannerMappings(Set.empty, List(0), scanners)
    transformations.foreach(t => println(s"${t._1} -> ${t._2} = ${t._3}"))
    -1
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day19 extends App {
  new Day19().solvePuzzles("/2021/day19.txt")
}
