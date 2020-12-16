package nl.njtromp.adventofcode_2020

import scala.collection.mutable

class Day16 extends Puzzle {
  private val Rule = "(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
  private val MyTicket = "your ticket:".r
  private val NearbyTickets = "nearby tickets:".r

  def solvePart1(lines: List[String]): Long = {
    var rules: mutable.HashMap[String, List[(Int, Int)]] = mutable.HashMap.empty
    var myTicket: List[Int] = List.empty
    var nearbyTickets: List[List[Int]] = List(List.empty)
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
    val rs: List[(Int, Int)] = rules.values.toList.flatMap(rs => rs)
    val ns: List[Int] = nearbyTickets.flatMap(ns => ns)
    ns.filter(n => !rs.exists(r => n >= r._1 && n <= r._2)).sum
  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

}

object Day16 extends App {
  new Day16().solvePuzzles("/input-puzzle16.txt")
}
