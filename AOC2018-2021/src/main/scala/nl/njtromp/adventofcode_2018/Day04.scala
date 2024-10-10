package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.util.matching.Regex

class Day04 extends Puzzle {
  val NewGuard: Regex = "\\[(.+)] Guard #(\\d+) begins shift".r
  val FallsAsleep: Regex = "\\[(.+)] falls asleep".r
  val WakesUp: Regex = "\\[(.+)] wakes up".r

  type GuardId = Int
  sealed abstract class Guard {
    def dateTime(): LocalDateTime;
  }
  case class Start(id: GuardId, start: LocalDateTime) extends Guard {
    override def dateTime(): LocalDateTime = start
  }
  case class Asleep(asleep: LocalDateTime) extends Guard {
    override def dateTime(): LocalDateTime = asleep
  }
  case class Awake(awake: LocalDateTime) extends Guard {
    override def dateTime(): LocalDateTime = awake
  }

  def mergeMaps[K, V](map1: Map[K, V], map2: Map[K, V], merge: (V, V) => V): Map[K, V] =
    (map1.keySet ++ map2.keySet).map { key=> (map1.contains(key), map2.contains(key)) match {
      case (true, false) => (key -> map1(key))
      case (false, true) => (key -> map2(key))
      case (true, true) => (key -> merge(map1(key), map2(key)))
      case (false, flase) => (key -> map1(key))
    }}.toMap

  override def solvePart1(lines: List[String]): Long = {
    val dateTimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    val events: List[Guard] = lines.sorted.map({
        case NewGuard(dateTime, id) => Start(id.toInt, LocalDateTime.parse(dateTime, dateTimeFormat))
        case FallsAsleep(dateTime) => Asleep(LocalDateTime.parse(dateTime, dateTimeFormat))
        case WakesUp(dateTime) => Awake(LocalDateTime.parse(dateTime, dateTimeFormat))
    })
    val NO_DATE_TIME: Option[LocalDateTime] = None
    val sleepingData = events.foldLeft((Map.empty[GuardId, Map[LocalDate, Set[Int]]], 0, NO_DATE_TIME))((gd, e) => e match {
      // New guard starting
      case Start(id, dateTime) =>
        // Check if another guard is currently asleep
        gd._3 match {
          // There is no other guard sleeping
          case None => if (gd._1.contains(id)) {
              // We already know this guard
              (gd._1, id, NO_DATE_TIME)
            } else {
              // A completely new guard, lets register him
              val dutyDate = if (dateTime.getHour != 0) dateTime.plusHours(1).toLocalDate else dateTime.toLocalDate
              (gd._1 ++ Map(id -> Map.empty[LocalDate, Set[Int]]), id, NO_DATE_TIME)
            }
          // There is an other guard sleeping
          case Some(sleepStarted) => {
            (gd._1 ++ Map(gd._2 -> (gd._1(gd._2) ++ Map(dateTime.toLocalDate -> (sleepStarted.getMinute until dateTime.getMinute).toSet))), gd._2, NO_DATE_TIME)
          }
        }
      case Asleep(dateTime) =>
        (gd._1, gd._2, Some(dateTime))
      case Awake(dateTime) =>
        val newMinutes = Map(dateTime.toLocalDate -> (gd._3.get.getMinute until dateTime.getMinute).toSet)
        val bla = gd._1(gd._2) ++ newMinutes
        (gd._1 ++ Map(gd._2 -> bla), gd._2, NO_DATE_TIME)
    })._1
    val guardId = sleepingData.maxBy(g => g._2.values.map(_.size).sum)._1
    sleepingData(guardId).foreach(x => println(s"${x._1} -> ${x._2.toList.sorted}"))
    val sleepiestMinutes = (0 to 59).map(d => d -> sleepingData(guardId).values.count(_.contains(d)))
    val sleepingMinute = sleepiestMinutes.maxBy(_._2)._1
    guardId * sleepingMinute + 1
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day04 extends App {
  new Day04().solvePuzzles("/2018/day04.txt")
}
