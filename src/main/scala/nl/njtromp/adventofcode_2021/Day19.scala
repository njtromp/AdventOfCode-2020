package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.{Matrix, Puzzle}

import scala.annotation.tailrec
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

  def matchBeacons(beacons: List[Matrix], others: List[Matrix]): List[((Matrix, Matrix), (Matrix, Matrix))] = {
    beacons
      .flatMap(b1_1 => beacons.filterNot(b1_1.eq(_)).map(b1_2 => ((b1_1, b1_2), findMatching(others, b1_1, b1_2))))
      .filter(_._2.nonEmpty)
      .map(m => (m._1, m._2.head))
  }

  def findTransformations(base: Scanner, scanners: List[Scanner]): List[Scanner] = {
    scanners.map(s => Matrix.perpendicularPermutations.foldLeft(Option.empty[Set[Matrix]])((acc, t) => acc match {
        case bs: Some[Set[Matrix]] =>
          bs
        case None =>
          val transformedBeacons = s.beacons.map(t * _)
          val matchingInfo = matchBeacons(base.beacons.toList, transformedBeacons.toList)
          if (matchingInfo.size < 24) // Take into account that we find matches both ways! Hence the 24 (=2 * 12)
            None
          else {
            val offset = matchingInfo.head._1._1 - matchingInfo.head._2._1
            Some(transformedBeacons.map(_ + offset))
          }
      }) match {
        case None => s
        case Some(m) => Scanner(s.id, m)
      }
    )
  }

  @tailrec
  private def findScannerMappings(connectedScanners: Set[Int], needInvestigation: Set[Int], scanners: List[Scanner]): List[Scanner] = {
    if (needInvestigation.isEmpty)
      scanners
    else {
      val base = scanners.find(_.id == needInvestigation.head).get
      val unmapped = scanners.filterNot(s => connectedScanners.contains(s.id) || needInvestigation.contains(s.id))
      val possiblyMapped = findTransformations(base, unmapped)
      val mappedScanners = possiblyMapped.zip(unmapped).filterNot(s => s._1.beacons == s._2.beacons).map(_._1)
      val findConnectionsFor = (needInvestigation.tail ++ mappedScanners.map(_.id).toSet).diff(connectedScanners)
      val correctedScanners = scanners.filterNot(s => mappedScanners.map(_.id).contains(s.id)) ++ mappedScanners
      findScannerMappings(connectedScanners + base.id, findConnectionsFor, correctedScanners)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val scanners = parseScannerInfo(lines).reverse
    val alignedScanners = findScannerMappings(Set.empty, Set(0), scanners)
    alignedScanners.flatMap(_.beacons).toSet.size
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day19 extends App {
  new Day19().solvePuzzles("/2021/day19.txt")
}
