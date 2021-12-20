package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.{Matrix, Puzzle}

import scala.util.matching.Regex

class Day19 extends Puzzle {

  val ScannerLine: Regex = raw"--- scanner (\d+) ---".r
  type Pos = List[Int]

  case class Scanner(id: Int, beacons: Set[Matrix]) {
    def intersect(s: Scanner): Set[Matrix] = beacons.intersect(s.beacons)
    def relativeLocations: Set[Matrix] = {
      val beaconList = beacons.toList
      val relativeBeacons: List[Matrix] = beaconList.flatMap(b0 =>
        beaconList.map(b => b - b0)
      )
      relativeBeacons.toSet
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

  def findTransformations(scanners: List[Scanner]): List[Option[Matrix]] = {
    val scanner0 = scanners.head
    val conversionForScanner = scanners.tail.map(s => {
      Matrix.permutations.map(_.round.clean).find(p => s.beacons.map(p * _).map(_.round.clean).intersect(scanner0.beacons).size >= 12)
    })
    conversionForScanner
  }

  override def solvePart1(lines: List[String]): Long = {
    val scanners = parseScannerInfo(lines)
//    val scanner0 = scanners.head
//    println(scanner0)
//    println("="*20)
//    val conversionForScanner = scanners.tail.map(s => {
//      Matrix.permutations.map(_.round.clean).find(p => s.beacons.map(p * _).map(_.round.clean).intersect(scanner0.beacons).size >= 6)
//    })
//    conversionForScanner.foreach(println)
    scanners.foreach(s => {
      val locations = s.relativeLocations
      println(s"${s.id} => ${locations.size}")
    })
    findTransformations(scanners.map(s => Scanner(s.id, s.relativeLocations))).foreach(println)
    -1
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day19 extends App {
  new Day19().solvePuzzles("/2021/day19.txt")
}
