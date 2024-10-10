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

  def findTransformations(base: Scanner, scanners: List[Scanner]): List[(Scanner, Scanner, Option[Matrix])] = {
    val conversionForScanner = scanners.filterNot(_ == base).map(s => {
      (base, s, Matrix.permutations.map(_.round.clean).find(p => s.beacons.map(p * _).map(_.round.clean).intersect(base.beacons).size >= 100))
    })
    conversionForScanner
  }

  def findScannerMappings(connectedScanners: List[Scanner], scanners: List[Scanner]): List[(Int, Int, Matrix)] = {
    if (scanners.isEmpty)
      List.empty
    else {
      val correctedScanners = connectedScanners.flatMap(s => findTransformations(s, scanners))
        .filter(_._3.isDefined)
        .map(s => (s._1, s._2, s._3.get))
        .map(s => (s._1, s._2.transform(s._3), s._3)
        ).distinct
      val remainingScanners = scanners.filterNot(s => correctedScanners.exists(_._2.id == s.id))
      correctedScanners.map(s => (s._1.id, s._2.id, s._3)) ++ findScannerMappings(correctedScanners.map(_._2), remainingScanners)
    }
  }

  def sqr(n: Int): Int = n * n

  def error(beacons1: List[(Int, Int, Int)], beacons2: List[(Int, Int, Int)]): Long =
    beacons1.flatMap(b0 => beacons2.map(b1 => sqr(b0._1 - b1._1) + sqr(b0._2 - b1._2) + sqr(b0._3 - b1._3))).sum

  def findMinimum(beacons1: List[(Int, Int, Int)], beacons2: List[(Int, Int, Int)]): Unit = {
    // 58, 55, 129
    val delta = (1, 0, 0)
    var beacons = beacons2.map(b => (b._1 - 55, b._2 + 52, b._3 + 129))
    println(beacons1)
    println(beacons)
    var previousError = 0L
    var lastError = Long.MaxValue
    while {
      previousError = lastError
      beacons = beacons.map(b => (b._1 + delta._1, b._2 + delta._2, b._3 + delta._3))
      lastError = error(beacons1, beacons)
      println(s"$lastError - $previousError")
      lastError < previousError
    } do()
//    do {
//      previousError = lastError
//      beacons = beacons.map(b => (b._1 + delta._1, b._2 + delta._2, b._3 + delta._3))
//      lastError = error(beacons1, beacons)
//      println(s"$lastError - $previousError")
//    } while (lastError < previousError)
    println(beacons2.head._1 - beacons.head._1)

  }

  override def solvePart1(lines: List[String]): Long = {
    val scanners = parseScannerInfo(lines)
    val relativeScanners = scanners.map(s => Scanner(s.id, s.relativeLocations))
    val mappings = findScannerMappings(relativeScanners.filter(_.id == 0), relativeScanners.filterNot(_.id == 0))
    val edges = mappings.map(m => (m._1, m._2))
    val correctedScanners = scanners.map(s => if (s.id == 0) s else s.transform(mappings.filter(_._2 == s.id).head._3))
    println(edges)
    println(correctedScanners.size)
    correctedScanners.foreach(s => println(s"${s.id} - ${s.beacons.size} (${s.beacons.size * (s.beacons.size - 1)}) ${s.relativeLocations.size}"))
    // 699 is to high
    // 934 should also be too high, that is the total number of beacons found in the scans assuming they don't overlap!
//    val bla: List[List[(Int, Int, Int)]] = correctedScanners.map(_.beacons.toList.map(_.toTuple))
//    val s0 = bla.head
//    val s1 = bla(1)
//    val s2 = (correctedScanners(1).beacons.map(b => b + Matrix(List(List(-1), List(1), List(1))))).toList.map(_.toTuple)
//    val e01 = s0.flatMap(b0 => s1.map(b1 => sqr(b0._1 - b1._1) + sqr(b0._2 - b1._2) + sqr(b0._3 - b1._3))).sum
//    val e02 = s0.flatMap(b0 => s2.map(b2 => sqr(b0._1 - b2._1) + sqr(b0._2 - b2._2) + sqr(b0._3 - b2._3))).sum
//    if (e02 < e01) println("Better") else if (e02 > e01) println("Worst") else println("equal")
//    println(s"$e01 : $e02")
//    findMinimum(s0, s1)

    -1
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day19 extends App {
  new Day19().solvePuzzles("/2021/day19.txt")
}
