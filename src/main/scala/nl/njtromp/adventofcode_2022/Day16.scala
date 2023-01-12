package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day16 extends Puzzle2 {
  private val VALVE = """Valve (\w{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r

  case class Valve(index: Int, id: String, flowRate: Int, connections: List[String]) {
//    var visited: Boolean = false
    override def toString: String = s"Valve $id, $flowRate $connections"
  }

  private def readValveInfo(lines: List[String]): Map[String, Valve] = {
    var index = -1
    lines.map(l => {
      index += 1
      l match {
        case VALVE(id, flowRate, connections) => id -> Valve(index, id,  flowRate.toInt, connections.replaceAll(" ", "").split(',').toList)
      }
    }).toMap
  }

  private def findRoute(valves: Map[String, Valve], start: String, finish: String): Int = {
    val distance = mutable.Map[String, Int]().withDefaultValue(Integer.MAX_VALUE)
    val source = mutable.Map[String, String]()
    val visited = mutable.Set[String]()
    val investigate = ArrayBuffer[String]()
    @tailrec
    def dijkstra(): Unit = {
      if (investigate.nonEmpty) {
        val current = investigate.minBy(distance)
        investigate -= current
        visited += current
        val neighbors = valves(current).connections
        neighbors.foreach(n => {
          if (distance(current) +  1 < distance(n)) {
            distance(n) = distance(current) + 1
            source += n -> current
          }
          if (!visited.contains(n) && !investigate.contains(n)) {
            investigate += n
          }
        })
        dijkstra()
      }
    }

    distance(start) = 0
    investigate += start
    dijkstra()
    distance(finish)
  }

  override def exampleAnswerPart1: Long = 1651
  override def solvePart1(lines: List[String]): Long = {
    val totalTime = 30
    val valves = readValveInfo(lines)
    val valvesToOpen = valves.filter(_._2.flowRate > 0).keys.toList
    val distanceValves = "AA" :: valvesToOpen
    // Holds the time needed to travel between the valves that needs to be opened
    val travelTime: Map[(String, String), Int] = distanceValves.flatMap(s => distanceValves.filterNot(_ == s).map(f => ((s, f), findRoute(valves, s, f)))).toMap
    var maxReleased = 0L
    def optimizeOpening(time: Int, myValve: String, myOpenTime: Int, needsOpening: List[String], released: Int): Unit = {
      if (time < totalTime) {
        val myRelease = if (time == myOpenTime) valves(myValve).flowRate * (totalTime - myOpenTime - 1) else 0
        val nowReleased = released + myRelease
        if (nowReleased > maxReleased) {
          maxReleased = nowReleased
        }
        if (time == myOpenTime) {
          needsOpening.foreach(v => optimizeOpening(
            time + 1,
            v, time + 1 + travelTime(myValve, v),
            needsOpening.filterNot(_ == v),
            nowReleased)
          )
        } else {
          optimizeOpening(time + 1, myValve, myOpenTime, needsOpening, released)
        }
      }
    }
    valvesToOpen.foreach(v => optimizeOpening(
        0,
        v, travelTime("AA", v),
        valvesToOpen.filterNot(_ == v),
        0)
    )
    maxReleased
  }

  override def exampleAnswerPart2: Long = 1707
  override def solvePart2(lines: List[String]): Long = {
    val totalTime = 26
    val valves = readValveInfo(lines)
    val valvesToOpen = valves.filter(_._2.flowRate > 0).keys.toList
    val distanceValves = "AA" :: valvesToOpen
    // Holds the time needed to travel between the valves that needs to be opened
    val travelTime: Map[(String, String), Int] = distanceValves.flatMap(s => distanceValves.filterNot(_ == s).map(f => ((s, f), findRoute(valves, s, f)))).toMap
    var maxReleased = 0L
    def optimizeOpening(time: Int, myValve: String, myOpenTime: Int, hisValve: String, hisOpenTime: Int, needsOpening: List[String], released: Int): Unit = {
      if (time < totalTime &&
        (myOpenTime < totalTime || hisOpenTime < totalTime ||
        (released + needsOpening.map(valves(_).flowRate).sum * (totalTime - time) ) > maxReleased)) {
        val myRelease = if (time == myOpenTime) valves(myValve).flowRate * (totalTime - myOpenTime - 1) else 0
        val hisRelease = if (time == hisOpenTime) valves(hisValve).flowRate * (totalTime - hisOpenTime - 1) else 0
        val nowReleased = released + myRelease + hisRelease
        if (nowReleased > maxReleased) {
          maxReleased = nowReleased
          println(maxReleased)
        }
        if (time == myOpenTime && time == hisOpenTime) {
          needsOpening.foreach(m => {
            needsOpening.filterNot(_ == m).foreach(h => {
              val myNewOpenTime = time + 1 + travelTime(myValve, m)
              val hisNewOpenTime = time + 1 + travelTime(hisValve, h)
              optimizeOpening(
                Math.min(myNewOpenTime, hisNewOpenTime),
                m, myNewOpenTime,
                h, hisNewOpenTime,
                needsOpening.filterNot(v => v == m || v == h),
                nowReleased
              )
            })
          })
        } else if (time == myOpenTime) {
          needsOpening match {
            case Nil => optimizeOpening(
              hisOpenTime,
              "", totalTime + 1, // No more valves to open
              hisValve, hisOpenTime,
              needsOpening,
              nowReleased
            )
            case _ => needsOpening.foreach(v => {
              val myNewOpenTime = time + 1 + travelTime(myValve, v)
              optimizeOpening(
                Math.min(time + 1, myNewOpenTime),
                v, myNewOpenTime,
                hisValve, hisOpenTime,
                needsOpening.filterNot(_ == v),
                nowReleased
              )
            })
          }
        } else if (time == hisOpenTime) {
          needsOpening match {
            case Nil => optimizeOpening(
              myOpenTime,
              myValve, myOpenTime,
              "", totalTime + 1,
              needsOpening,
              nowReleased
            )
            case _ => needsOpening.foreach(v => {
              val hisNewOpenTime = time + 1 + travelTime(hisValve, v)
              optimizeOpening(
                Math.min(time + 1, hisNewOpenTime),
                myValve, myOpenTime,
                v, hisNewOpenTime,
                needsOpening.filterNot(_ == v),
                nowReleased
              )
            })
          }
        } else {
          optimizeOpening(time + 1, myValve, myOpenTime, hisValve, hisOpenTime, needsOpening, released)
        }
      }
    }
    valvesToOpen.foreach(m => {
      valvesToOpen.filterNot(_ == m).foreach(h => {
//        println(s"Starting with $m, $h\n")
        val myOpenTime = travelTime("AA", m)
        val hisOpenTime = travelTime("AA", h)
        optimizeOpening(
          Math.min(myOpenTime, hisOpenTime),
          m, myOpenTime,
          h, hisOpenTime,
          valvesToOpen.filterNot(v => v == m || v == h),
          0)
      })
    })
    maxReleased
  }

}

object Day16 extends App{
  new Day16().solvePuzzles("/2022/day16.txt")
}
