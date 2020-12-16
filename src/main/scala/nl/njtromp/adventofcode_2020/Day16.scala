package nl.njtromp.adventofcode_2020

import scala.collection.mutable

class Day16 extends Puzzle {
  private val Rule = "(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
  private val MyTicket = "your ticket:".r
  private val NearbyTickets = "nearby tickets:".r

  private var rules: mutable.HashMap[String, List[(Int, Int)]] = mutable.HashMap.empty
  private var myTicket: List[Int] = List.empty
  private var nearbyTickets: List[List[Int]] = List(List.empty)

  def solvePart1(lines: List[String]): Long = {
    processData(lines)
    nearbyTickets.flatten.filter(n => !rules.values.toList.flatten.exists(r => n >= r._1 && n <= r._2)).sum
  }

  def solvePart2(lines: List[String]): Long = {
    val fieldOrder: List[String] = determineFieldOrder(lines)
    fieldOrder.zipWithIndex.filter(f => f._1.startsWith("departure")).map(f => myTicket(f._2).toLong).product
  }

  def determineFieldOrder(lines: List[String]): List[String] = {
    processData(lines)
    val validTickets = nearbyTickets.filter(ns => ns.forall(n => rules.values.toList.flatten.exists(r => n >= r._1 && n <= r._2))).filter(_.nonEmpty)
    val possibleFields = myTicket.indices.map(i =>
      rules.filter(r =>
        validTickets.forall(t =>
          r._2.exists(
            range => t(i) >= range._1 && t(i) <= range._2
          )
        )
      ).keys.toList
    ).toArray
    val knownFields: mutable.Set[Int] = mutable.Set.empty
    while (knownFields.size != rules.size) {
      possibleFields.zipWithIndex.filter(pf => pf._1.size == 1).foreach({case (names, pos) =>
        if (!knownFields.contains(pos)) {
          knownFields += pos
          possibleFields.zipWithIndex.foreach({case (fields, i) => if (i != pos) possibleFields(i) = fields.filter(_ != names.head)})
        }
      })
    }
    possibleFields.flatten.toList
  }

  private def processData(lines: List[String]): Unit = {
    rules = mutable.HashMap.empty
    myTicket = List.empty
    nearbyTickets = List(List.empty)
    var scanningState: Int = 0;
    for (line <- lines) {
      if (line.nonEmpty) {
        if (scanningState == 0) {
          line match {
            case Rule(name, l1, u1, l2, u2) =>
              rules += (name -> List((l1.toInt, u1.toInt), (l2.toInt, u2.toInt)))
            case MyTicket() => scanningState = 1
          }
        } else if (scanningState == 1) {
          line match {
            case NearbyTickets() => scanningState = 2
            case _ => myTicket = line.split(",").map(_.toInt).toList
          }
        } else {
          nearbyTickets = line.split(",").map(_.toInt).toList +: nearbyTickets
        }
      }
    }
  }

}

object Day16 extends App {
  new Day16().solvePuzzles("/input-puzzle16.txt")
}
